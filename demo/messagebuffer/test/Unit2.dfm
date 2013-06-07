object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 248
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 168
    Top = 24
    Width = 118
    Height = 13
    Caption = 'Total Messages in Buffer'
  end
  object lblTotal: TLabel
    Left = 184
    Top = 56
    Width = 65
    Height = 25
    Caption = '10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 33023
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 24
    Width = 58
    Height = 13
    Caption = 'Total Writes'
  end
  object lblTotalWrites: TLabel
    Left = 17
    Top = 56
    Width = 65
    Height = 25
    Caption = '10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 33023
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 365
    Top = 24
    Width = 57
    Height = 13
    Caption = 'Total Reads'
  end
  object lblTotalReads: TLabel
    Left = 358
    Top = 56
    Width = 65
    Height = 25
    Caption = '10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 33023
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label7: TLabel
    Left = 17
    Top = 136
    Width = 63
    Height = 13
    Caption = 'Write Speed:'
  end
  object Label8: TLabel
    Left = 281
    Top = 136
    Width = 62
    Height = 13
    Caption = 'Read Speed:'
  end
  object lblReadDelays: TLabel
    Left = 496
    Top = 56
    Width = 65
    Height = 25
    Caption = '10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 33023
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 496
    Top = 24
    Width = 85
    Height = 13
    Caption = 'Read Delays (MS)'
  end
  object lblWriteMPS: TLabel
    Left = 8
    Top = 87
    Width = 104
    Height = 14
    Caption = 'Actual MPS: 10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblReadMPS: TLabel
    Left = 349
    Top = 87
    Width = 35
    Height = 14
    Caption = '10000'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object cboWriteSpeed: TComboBox
    Left = 86
    Top = 133
    Width = 67
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      '1000'
      '2000'
      '4000'
      '5000'
      '10000')
  end
  object cboReadSpeed: TComboBox
    Left = 365
    Top = 133
    Width = 67
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      '1000'
      '2000'
      '4000'
      '5000'
      '10000')
  end
  object btnStart: TButton
    Left = 17
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 113
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = btnStopClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 208
  end
end
