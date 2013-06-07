object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 307
  ClientWidth = 549
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
    Left = 8
    Top = 251
    Width = 98
    Height = 13
    Caption = 'Total Message Size: '
  end
  object Label2: TLabel
    Left = 8
    Top = 65
    Width = 90
    Height = 13
    Caption = 'Encoding Options: '
  end
  object btnEncode: TButton
    Left = 8
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Encode'
    TabOrder = 5
    OnClick = btnEncodeClick
  end
  object txtName: TLabeledEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    TabOrder = 0
  end
  object txtDob: TLabeledEdit
    Left = 280
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 115
    EditLabel.Height = 13
    EditLabel.Caption = 'DOB (e.g. 1980-01-01):'
    TabOrder = 2
  end
  object txtAge: TLabeledEdit
    Left = 144
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 23
    EditLabel.Height = 13
    EditLabel.Caption = 'Age:'
    TabOrder = 1
  end
  object txtHeight: TLabeledEdit
    Left = 416
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 91
    EditLabel.Height = 13
    EditLabel.Caption = 'Height (e.g. 1.78):'
    TabOrder = 3
  end
  object hexViewer: TMPHexEditor
    Left = 8
    Top = 125
    Width = 529
    Height = 120
    Cursor = crIBeam
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    BytesPerRow = 16
    Translation = tkAsIs
    OffsetFormat = '-!10:0x|'
    Colors.Background = clWindow
    Colors.ChangedBackground = 11075583
    Colors.ChangedText = clMaroon
    Colors.CursorFrame = clNavy
    Colors.Offset = clBlack
    Colors.OddColumn = clBlue
    Colors.EvenColumn = clNavy
    Colors.CurrentOffsetBackground = clBtnShadow
    Colors.OffsetBackground = clBtnFace
    Colors.CurrentOffset = clBtnHighlight
    Colors.Grid = clBtnFace
    Colors.NonFocusCursorFrame = clAqua
    Colors.ActiveFieldBackground = clWindow
    FocusFrame = True
    DrawGridLines = False
    ReadOnlyView = True
    Version = 'september 30, 2007; ?markus stephany, vcl[at]mirkes[dot]de'
  end
  object btnLoad: TButton
    Left = 462
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 8
    OnClick = btnLoadClick
  end
  object btnDecode: TButton
    Left = 89
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Decode'
    TabOrder = 6
    OnClick = btnDecodeClick
  end
  object btnSave: TButton
    Left = 381
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object chkCompressed: TCheckBox
    Left = 120
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Compressed'
    TabOrder = 4
    OnClick = chkCompressedClick
  end
  object Button1: TButton
    Left = 170
    Top = 94
    Width = 75
    Height = 25
    Caption = 'DecodeTest'
    TabOrder = 10
    OnClick = Button1Click
  end
  object chkNamedKey: TCheckBox
    Left = 223
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Named Key'
    TabOrder = 11
    OnClick = chkNamedKeyClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'shape.message'
    Filter = 'Shape Messages|*.shape.message'
    Left = 416
    Top = 264
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'shape.message'
    Filter = 'Shape Messages|*.shape.message'
    Left = 448
    Top = 264
  end
end
