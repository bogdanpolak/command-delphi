object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 309
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 303
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    ExplicitHeight = 236
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 55
      Width = 175
      Height = 18
      Align = alTop
      Shape = bsSpacer
    end
    object Bevel2: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 146
      Width = 175
      Height = 18
      Align = alTop
      Shape = bsSpacer
      ExplicitLeft = 7
      ExplicitTop = 170
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 175
      Height = 31
      Align = alTop
      Caption = 'Button1'
      TabOrder = 0
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 79
      Width = 175
      Height = 34
      Align = alTop
      Caption = 'Button2'
      TabOrder = 1
    end
    object Edit1: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 119
      Width = 175
      Height = 21
      Align = alTop
      TabOrder = 2
      Text = 'Edit1'
    end
    object btnExecuteTwoCommands: TButton
      AlignWithMargins = True
      Left = 5
      Top = 170
      Width = 175
      Height = 34
      Align = alTop
      Caption = 'btnExecuteTwoCommands'
      TabOrder = 3
      OnClick = btnExecuteTwoCommandsClick
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 220
      Width = 175
      Height = 17
      Margins.Top = 13
      Align = alTop
      Caption = 'Enable command Button2'
      Checked = True
      State = cbChecked
      TabOrder = 4
      ExplicitLeft = 24
      ExplicitTop = 248
      ExplicitWidth = 97
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 194
    Top = 3
    Width = 359
    Height = 303
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
    ExplicitWidth = 323
    ExplicitHeight = 236
  end
end
