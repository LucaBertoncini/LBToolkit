unit uLBAIConnectorTypes;
{
  Types shared between the AI connector (uLBAIConnector.pas) and the classes that implement the
  functions exposed to an AI.

  WHO DOES WHAT
  ----------------------------------------------------------------------
  Routes.xml declares every endpoint ONCE, for the web application and for the AI alike (URI, OpType
  and a Description). One endpoint with Kind="AIConnector" is the entry point of the AI (MCP). Every
  authenticated Standard endpoint is a candidate tool. The class that implements the function (a
  TRouteHandlerBase descendant, registered with RegisterStandardWorker as always) may also implement
  IAIConnectorFunctions to say WHICH PARAMETERS the function needs. Functions that are not described
  are either not exposed (default) or exposed with free-form parameters (connector's
  ExposeUndescribed). The user's role always decides what may be run.

  Example (inside a handler class):

    function TMyHandlers.DescribeAIFunction(const aFunctionName: String;
      aDescriptor: TAIFunctionDescriptor): Boolean;
    begin
      Result := True;
      if SameText(aFunctionName, 'SearchOrders') then
      begin
        aDescriptor.Effect := aeReadOnly;
        aDescriptor.AddParam('text', aptString, 'Words to look for', False);
        aDescriptor.AddEnumParam('status', 'Order status', 'open,closed', False);
        aDescriptor.AddParam('max_rows', aptInteger, 'Maximum number of rows (default 50)', False);
      end
      else
        Result := False;   // no precise description for this function
    end;

  The interface is a CORBA interface: no reference counting, it can be added to a plain
  TObject descendant such as TRouteHandlerBase.
}

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, fgl, fpjson, uLBAuthContext;

type
  TAIParamType = (aptString, aptInteger, aptNumber, aptBoolean, aptDate, aptDateTime, aptObject, aptArray);

  {:What the function does to the data, as a hint for the AI client (MCP "annotations"). Clients may
    use it to decide whether to ask the user before the call. aeUnspecified sends no hint.}
  TAIToolEffect = (aeUnspecified, aeReadOnly, aeModifies, aeDestructive);

  { TAIAuthenticationContext }

  {:Old name of TLBAuthenticationContext (uLBAuthContext.pas, in utils): the class lives where the token manager
    that defines it lives, so that utils does not depend on this folder. The two names are the SAME type.}
  TAIAuthenticationContext = TLBAuthenticationContext;

  {:Authentication of an AI request. On success anAuthentication must be a NEW context; the
    connector frees it after the request. The context, not a JSON copy, guarantees the lifetime of
    UserConfig.}
  TAIAuthenticateEvent = function(requestHeaders, responseHeaders: TStringList;
    out anAuthentication: TAIAuthenticationContext): Boolean of object;

  { TAIParamDescriptor }

  TAIParamDescriptor = class(TObject)
  public
    Name        : String;
    Description : String;
    ParamType   : TAIParamType;
    Required    : Boolean;
    ItemType    : TAIParamType;     // significant for aptArray only
    EnumValues  : TStringList;      // allowed values (strings); empty = any value

    constructor Create;
    destructor Destroy; override;

    function ToJSONSchema(): TJSONObject;
    {:Checks (and, for numbers and booleans sent as text, converts in place) the value.}
    function ValidateValue(var aValue: TJSONData; out anError: String): Boolean;
  end;

  TAIParamList = specialize TFPGObjectList<TAIParamDescriptor>;

  { TAIFunctionDescriptor }

  TAIFunctionDescriptor = class(TObject)
  strict private
    FFunctionName : String;
    FParams       : TAIParamList;
  public
    {:Used when Routes.xml gives no Description for the endpoint.}
    Description   : String;
    {:Optional human title for the AI client.}
    Title         : String;
    Effect        : TAIToolEffect;
    {:Set by DescribeAIFunction to keep a function away from the AI whatever the role (rare).}
    Hidden        : Boolean;
    {:Set by the connector for a function nobody described: any JSON object is accepted and the
      Routes.xml Description is all the AI knows. Describing the parameters is always better.}
    FreeForm      : Boolean;

    constructor Create(const aFunctionName: String);
    destructor Destroy; override;

    function AddParam(const aName: String; aType: TAIParamType; const aDescription: String;
      aRequired: Boolean = False): TAIParamDescriptor;
    function AddArrayParam(const aName: String; anItemType: TAIParamType; const aDescription: String;
      aRequired: Boolean = False): TAIParamDescriptor;
    {:A string parameter limited to a set of values (comma separated list).}
    function AddEnumParam(const aName, aDescription, aCommaSeparatedValues: String;
      aRequired: Boolean = False): TAIParamDescriptor;

    {:JSON Schema ("type":"object") for the tool input: NEW object, owned by the caller.}
    function BuildInputSchema(): TJSONObject;
    {:Validates the arguments sent by the AI; the message is written for the AI, so that it can fix the call.}
    function ValidateArguments(anArguments: TJSONObject; out anError: String): Boolean;

    property FunctionName : String read FFunctionName;
    property Params       : TAIParamList read FParams;
  end;

  {:Implemented by the handler classes that expose functions to an AI (Kind="AIConnector").}
  IAIConnectorFunctions = interface
    ['{7D0C3F4E-2A51-4B8E-9C5B-5E1D6A9F3B42}']
    {:Fills aDescriptor for the function and returns True. Returns False when the class has no
      description for aFunctionName: the function is then exposed with free-form parameters if the
      connector's ExposeUndescribed is True, otherwise it is not exposed. To keep a function away
      from the AI altogether set aDescriptor.Hidden := True.}
    function DescribeAIFunction(const aFunctionName: String; aDescriptor: TAIFunctionDescriptor): Boolean;
  end;

function AIParamTypeToJSONType(aType: TAIParamType): String;

implementation

uses
  StrUtils;

const
  cJSON_Type_String  = String('string');
  cJSON_Type_Integer = String('integer');
  cJSON_Type_Number  = String('number');
  cJSON_Type_Boolean = String('boolean');
  cJSON_Type_Object  = String('object');
  cJSON_Type_Array   = String('array');

function AIParamTypeToJSONType(aType: TAIParamType): String;
begin
  case aType of
    aptString, aptDate, aptDateTime : Result := cJSON_Type_String;
    aptInteger                      : Result := cJSON_Type_Integer;
    aptNumber                       : Result := cJSON_Type_Number;
    aptBoolean                      : Result := cJSON_Type_Boolean;
    aptObject                       : Result := cJSON_Type_Object;
    aptArray                        : Result := cJSON_Type_Array;
  else
    Result := cJSON_Type_String;
  end;
end;

function InvariantFormat(): TFormatSettings;
begin
  // JSON numbers use the point, whatever the locale of the server.
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := #0;
end;

function IsDigits(const aText: String; aFrom, aCount: Integer): Boolean;
var
  i: Integer;
begin
  Result := aFrom + aCount - 1 <= Length(aText);
  if Result then
    for i := aFrom to aFrom + aCount - 1 do
      if not (aText[i] in ['0'..'9']) then
        Exit(False);
end;

function LooksLikeISODate(const aText: String; aWithTime: Boolean): Boolean;
var
  _Y, _M, _D, _H, _N, _S, p : Integer;
  _Dummy                     : TDateTime;
begin
  // ISO 8601: YYYY-MM-DD, and for date-time T|space HH:MM[:SS[.fff]] with an optional Z or +HH:MM.
  // The calendar is checked (no 31 February); the time is required when aWithTime.
  Result := False;
  if (Length(aText) < 10) or (not IsDigits(aText, 1, 4)) or (aText[5] <> '-') or (not IsDigits(aText, 6, 2)) or
     (aText[8] <> '-') or (not IsDigits(aText, 9, 2)) then Exit;
  _Y := StrToInt(Copy(aText, 1, 4));
  _M := StrToInt(Copy(aText, 6, 2));
  _D := StrToInt(Copy(aText, 9, 2));
  if (_Y < 1) or (not TryEncodeDate(_Y, _M, _D, _Dummy)) then Exit;

  if Length(aText) = 10 then
    Exit(not aWithTime);
  if not aWithTime then Exit;

  if (not (aText[11] in ['T', 't', ' '])) or (not IsDigits(aText, 12, 2)) or (Length(aText) < 16) or
     (aText[14] <> ':') or (not IsDigits(aText, 15, 2)) then Exit;
  _H := StrToInt(Copy(aText, 12, 2));
  _N := StrToInt(Copy(aText, 15, 2));
  if (_H > 23) or (_N > 59) then Exit;
  p := 17;
  if (Length(aText) >= p) and (aText[p] = ':') then
  begin
    if not IsDigits(aText, p + 1, 2) then Exit;
    _S := StrToInt(Copy(aText, p + 1, 2));
    if _S > 59 then Exit;
    Inc(p, 3);
    if (Length(aText) >= p) and (aText[p] = '.') then
    begin
      Inc(p);
      if not IsDigits(aText, p, 1) then Exit;
      while (p <= Length(aText)) and (aText[p] in ['0'..'9']) do Inc(p);
    end;
  end;
  if p > Length(aText) then Exit(True);                         // no zone: local time of the application
  if (aText[p] in ['Z', 'z']) and (p = Length(aText)) then Exit(True);
  if (aText[p] in ['+', '-']) and (Length(aText) = p + 5) and IsDigits(aText, p + 1, 2) and (aText[p + 3] = ':') and
     IsDigits(aText, p + 4, 2) then
    Exit((StrToInt(Copy(aText, p + 1, 2)) <= 23) and (StrToInt(Copy(aText, p + 4, 2)) <= 59));
end;

{ TAIParamDescriptor }

constructor TAIParamDescriptor.Create;
begin
  inherited Create;
  EnumValues := TStringList.Create;
  EnumValues.CaseSensitive := True;
  ParamType := aptString;
  ItemType := aptString;
end;

destructor TAIParamDescriptor.Destroy;
begin
  FreeAndNil(EnumValues);
  inherited Destroy;
end;

function TAIParamDescriptor.ToJSONSchema(): TJSONObject;
var
  _Items : TJSONObject;
  _Enum  : TJSONArray;
  i      : Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('type', AIParamTypeToJSONType(ParamType));

  case ParamType of
    aptDate     : Result.Add('format', 'date');
    aptDateTime : Result.Add('format', 'date-time');
  end;

  if ParamType = aptArray then
  begin
    _Items := TJSONObject.Create;
    _Items.Add('type', AIParamTypeToJSONType(ItemType));
    Result.Add('items', _Items);
  end;

  if EnumValues.Count > 0 then
  begin
    _Enum := TJSONArray.Create;
    for i := 0 to EnumValues.Count - 1 do
      _Enum.Add(EnumValues[i]);
    Result.Add('enum', _Enum);
  end;

  if Description <> '' then
    Result.Add('description', Description);
end;

function TAIParamDescriptor.ValidateValue(var aValue: TJSONData; out anError: String): Boolean;
var
  _Text : String;
  _Int  : Int64;
  _Flt  : Double;
  _New  : TJSONData;
  i     : Integer;
  _Item, _OldItem : TJSONData;
  _ItemDesc : TAIParamDescriptor;
  _ItemError : String;
begin
  Result := False;
  anError := '';

  if (aValue = nil) or (aValue.JSONType = jtNull) then
  begin
    anError := Format('parameter "%s" must not be null', [Name]);
    Exit;
  end;

  case ParamType of
    aptString, aptDate, aptDateTime:
      begin
        if aValue.JSONType in [jtNumber, jtBoolean] then
          aValue := TJSONString.Create(aValue.AsString);   // tolerated: a number sent for a text parameter
        if aValue.JSONType <> jtString then
        begin
          anError := Format('parameter "%s" must be a string', [Name]);
          Exit;
        end;
        _Text := aValue.AsString;
        if (ParamType = aptDate) and (not LooksLikeISODate(_Text, False)) then
        begin
          anError := Format('parameter "%s" must be a valid date in the form YYYY-MM-DD', [Name]);
          Exit;
        end;
        if (ParamType = aptDateTime) and (not LooksLikeISODate(_Text, True)) then
        begin
          anError := Format('parameter "%s" must be a valid date and time in ISO 8601 form (YYYY-MM-DDTHH:MM:SS)', [Name]);
          Exit;
        end;
        if (EnumValues.Count > 0) and (EnumValues.IndexOf(_Text) < 0) then
        begin
          anError := Format('parameter "%s" must be one of: %s', [Name, EnumValues.CommaText]);
          Exit;
        end;
      end;

    aptInteger:
      begin
        if aValue.JSONType = jtString then
        begin
          // Some clients send numbers as text: accepted when the text is a plain integer.
          if TryStrToInt64(Trim(aValue.AsString), _Int) then
            aValue := TJSONInt64Number.Create(_Int);   // the caller replaces the old value with this one
        end;
        if aValue.JSONType <> jtNumber then
        begin
          anError := Format('parameter "%s" must be an integer', [Name]);
          Exit;
        end;
        if (TJSONNumber(aValue).NumberType = ntFloat) and (Frac(aValue.AsFloat) <> 0) then
        begin
          anError := Format('parameter "%s" must be an integer, not a decimal number', [Name]);
          Exit;
        end;
      end;

    aptNumber:
      begin
        if aValue.JSONType = jtString then
        begin
          if TryStrToFloat(Trim(aValue.AsString), _Flt, InvariantFormat()) then
            aValue := TJSONFloatNumber.Create(_Flt);
        end;
        if aValue.JSONType <> jtNumber then
        begin
          anError := Format('parameter "%s" must be a number', [Name]);
          Exit;
        end;
      end;

    aptBoolean:
      begin
        if aValue.JSONType = jtString then
        begin
          if SameText(Trim(aValue.AsString), 'true') then
            aValue := TJSONBoolean.Create(True)
          else if SameText(Trim(aValue.AsString), 'false') then
            aValue := TJSONBoolean.Create(False);
        end;
        if aValue.JSONType <> jtBoolean then
        begin
          anError := Format('parameter "%s" must be true or false', [Name]);
          Exit;
        end;
      end;

    aptObject:
      if aValue.JSONType <> jtObject then
      begin
        anError := Format('parameter "%s" must be an object', [Name]);
        Exit;
      end;

    aptArray:
      begin
        if aValue.JSONType <> jtArray then
        begin
          anError := Format('parameter "%s" must be an array', [Name]);
          Exit;
        end;
        // every element must have the declared item type (and numbers sent as text are converted)
        _ItemDesc := TAIParamDescriptor.Create;
        try
          _ItemDesc.Name := Name;
          _ItemDesc.ParamType := ItemType;
          for i := 0 to TJSONArray(aValue).Count - 1 do
          begin
            _Item := TJSONArray(aValue).Items[i];
            _OldItem := _Item;
            if not _ItemDesc.ValidateValue(_Item, _ItemError) then
            begin
              anError := Format('parameter "%s", item %d: %s', [Name, i + 1, _ItemError]);
              if _Item <> _OldItem then _Item.Free;
              Exit;
            end;
            if _Item <> _OldItem then
            begin
              TJSONArray(aValue).Delete(i);
              TJSONArray(aValue).Insert(i, _Item);
            end;
          end;
        finally
          _ItemDesc.Free;
        end;
      end;
  end;

  Result := True;
end;

{ TAIFunctionDescriptor }

constructor TAIFunctionDescriptor.Create(const aFunctionName: String);
begin
  inherited Create;
  FFunctionName := aFunctionName;
  FParams := TAIParamList.Create(True);
  Effect := aeUnspecified;
end;

destructor TAIFunctionDescriptor.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TAIFunctionDescriptor.AddParam(const aName: String; aType: TAIParamType;
  const aDescription: String; aRequired: Boolean): TAIParamDescriptor;
begin
  Result := TAIParamDescriptor.Create;
  Result.Name := aName;
  Result.ParamType := aType;
  Result.Description := aDescription;
  Result.Required := aRequired;
  FParams.Add(Result);
end;

function TAIFunctionDescriptor.AddArrayParam(const aName: String; anItemType: TAIParamType;
  const aDescription: String; aRequired: Boolean): TAIParamDescriptor;
begin
  Result := Self.AddParam(aName, aptArray, aDescription, aRequired);
  Result.ItemType := anItemType;
end;

function TAIFunctionDescriptor.AddEnumParam(const aName, aDescription, aCommaSeparatedValues: String;
  aRequired: Boolean): TAIParamDescriptor;
var
  _Values : TStringArray;
  i       : Integer;
begin
  Result := Self.AddParam(aName, aptString, aDescription, aRequired);
  _Values := aCommaSeparatedValues.Split([',']);
  for i := 0 to High(_Values) do
    if Trim(_Values[i]) <> '' then
      Result.EnumValues.Add(Trim(_Values[i]));
end;

function TAIFunctionDescriptor.BuildInputSchema(): TJSONObject;
var
  _Props    : TJSONObject;
  _Required : TJSONArray;
  i         : Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('type', cJSON_Type_Object);

  _Props := TJSONObject.Create;
  _Required := TJSONArray.Create;
  for i := 0 to FParams.Count - 1 do
  begin
    _Props.Add(FParams[i].Name, FParams[i].ToJSONSchema());
    if FParams[i].Required then
      _Required.Add(FParams[i].Name);
  end;
  Result.Add('properties', _Props);
  if _Required.Count > 0 then
    Result.Add('required', _Required)
  else
    _Required.Free;
  Result.Add('additionalProperties', FreeForm);
end;

function TAIFunctionDescriptor.ValidateArguments(anArguments: TJSONObject; out anError: String): Boolean;
var
  i, j    : Integer;
  _Param  : TAIParamDescriptor;
  _Idx    : Integer;
  _Value  : TJSONData;
  _Old    : TJSONData;
  _Known  : Boolean;
  _Msg    : String;
begin
  Result := False;
  anError := '';

  if anArguments = nil then
  begin
    anError := 'the arguments must be a JSON object';
    Exit;
  end;

  // unknown names: reported, so that the AI notices a misspelled parameter instead of silently losing it
  for i := 0 to anArguments.Count - 1 do
  begin
    if FreeForm then Break;
    _Known := False;
    for j := 0 to FParams.Count - 1 do
      if SameStr(FParams[j].Name, anArguments.Names[i]) then
      begin
        _Known := True;
        Break;
      end;
    if not _Known then
    begin
      anError := Format('unknown parameter "%s"', [anArguments.Names[i]]);
      Exit;
    end;
  end;

  for j := 0 to FParams.Count - 1 do
  begin
    _Param := FParams[j];
    _Idx := anArguments.IndexOfName(_Param.Name);

    if _Idx < 0 then
    begin
      if _Param.Required then
      begin
        anError := Format('the required parameter "%s" is missing', [_Param.Name]);
        Exit;
      end;
      Continue;
    end;

    _Old := anArguments.Items[_Idx];
    _Value := _Old;
    if not _Param.ValidateValue(_Value, _Msg) then
    begin
      if _Value <> _Old then
        _Value.Free;   // converted copy that did not pass the remaining checks
      anError := _Msg;
      Exit;
    end;
    if _Value <> _Old then
      anArguments.Items[_Idx] := _Value;   // converted copy replaces the original (the object frees the old one)
  end;

  Result := True;
end;

end.
