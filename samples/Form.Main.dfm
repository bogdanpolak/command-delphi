object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 360
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 354
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 75
      Width = 175
      Height = 3
      Margins.Top = 23
      Margins.Bottom = 23
      Align = alTop
      ExplicitTop = 55
    end
    object btnExecuteCommand: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 175
      Height = 31
      Align = alTop
      Caption = 'btnExecuteCommand'
      TabOrder = 0
      OnClick = btnExecuteCommandClick
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 104
      Width = 175
      Height = 34
      Align = alTop
      Caption = 'Button1'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Edit1: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 184
      Width = 175
      Height = 21
      Align = alTop
      TabOrder = 2
      Text = 'Edit1'
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 144
      Width = 175
      Height = 34
      Align = alTop
      Caption = 'Button2'
      TabOrder = 3
      OnClick = Button2Click
      ExplicitLeft = 7
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 211
      Width = 175
      Height = 17
      Align = alTop
      Caption = 'Enable command Button2'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 5
      Top = 251
      Width = 175
      Height = 34
      Margins.Top = 20
      Align = alTop
      Caption = 'Button3'
      TabOrder = 5
      OnClick = Button3Click
    end
    object chkShowProgressbar: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 291
      Width = 175
      Height = 17
      Align = alTop
      Caption = 'Show Progress Bar'
      TabOrder = 6
      OnClick = chkShowProgressbarClick
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 194
    Top = 3
    Width = 359
    Height = 354
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
