object frmMain: TfrmMain
  Left = 86
  Height = 184
  Top = 85
  Width = 240
  Caption = 'Test UBX Rover'
  ClientHeight = 184
  ClientWidth = 240
  LCLVersion = '8.8'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object Label1: TLabel
    Left = 16
    Height = 16
    Top = 30
    Width = 29
    Caption = 'Port:'
  end
  object edPort: TEdit
    Left = 80
    Height = 28
    Top = 24
    Width = 136
    TabOrder = 0
    Text = '/dev/ttyACM0'
  end
  object btnStart: TButton
    Left = 16
    Height = 25
    Top = 87
    Width = 75
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 141
    Height = 25
    Top = 88
    Width = 75
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
end
