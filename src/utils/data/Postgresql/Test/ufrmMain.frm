object frmMain: TfrmMain
  Left = 373
  Height = 174
  Top = 256
  Width = 295
  Caption = 'Test Postgresql'
  ClientHeight = 174
  ClientWidth = 295
  LCLVersion = '8.9'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object btnStart: TButton
    Left = 39
    Height = 25
    Top = 57
    Width = 75
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 152
    Height = 25
    Top = 56
    Width = 75
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
end
