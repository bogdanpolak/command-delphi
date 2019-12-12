object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 517
  ClientWidth = 626
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
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 207
    Top = 3
    Width = 416
    Height = 511
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
    ExplicitTop = -2
    ExplicitWidth = 346
    ExplicitHeight = 354
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 198
    Height = 511
    Align = alLeft
    Caption = ' '
    TabOrder = 0
    ExplicitTop = 8
    ExplicitHeight = 782
    object GroupBoxSimpleDemo: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 190
      Height = 77
      Align = alTop
      Caption = 'GroupBoxSimpleDemo'
      TabOrder = 0
      object btnExecuteCommand: TButton
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 180
        Height = 31
        Align = alTop
        Caption = 'btnExecuteCommand'
        TabOrder = 0
        OnClick = btnExecuteCommandClick
        ExplicitWidth = 175
      end
    end
    object GroupBoxButtonCommands: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 87
      Width = 190
      Height = 170
      Align = alTop
      Caption = 'GroupBoxButtonCommands'
      TabOrder = 1
      object Button1: TButton
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 180
        Height = 34
        Align = alTop
        Caption = 'Button1'
        TabOrder = 0
        OnClick = Button1Click
        ExplicitLeft = 10
        ExplicitTop = 26
      end
      object Button2: TButton
        AlignWithMargins = True
        Left = 5
        Top = 58
        Width = 180
        Height = 34
        Align = alTop
        Caption = 'Button2'
        TabOrder = 1
        OnClick = Button2Click
        ExplicitTop = 144
        ExplicitWidth = 175
      end
      object Edit1: TEdit
        AlignWithMargins = True
        Left = 5
        Top = 98
        Width = 180
        Height = 21
        Align = alTop
        TabOrder = 2
        Text = 'Edit1'
        ExplicitTop = 184
        ExplicitWidth = 175
      end
      object CheckBox1: TCheckBox
        AlignWithMargins = True
        Left = 5
        Top = 125
        Width = 180
        Height = 17
        Align = alTop
        Caption = 'Enable command Button2'
        Checked = True
        State = cbChecked
        TabOrder = 3
        ExplicitLeft = 3
        ExplicitTop = 203
      end
    end
    object GroupBoxDiceRolls: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 263
      Width = 190
      Height = 122
      Align = alTop
      Caption = 'Dice Rolls'
      TabOrder = 2
      object Button3: TButton
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 180
        Height = 34
        Align = alTop
        Caption = 'Button3'
        TabOrder = 0
        OnClick = Button3Click
        ExplicitTop = 251
        ExplicitWidth = 175
      end
      object chkShowProgressbar: TCheckBox
        AlignWithMargins = True
        Left = 5
        Top = 58
        Width = 180
        Height = 17
        Align = alTop
        Caption = 'Show Progress Bar'
        TabOrder = 1
        OnClick = chkShowProgressbarClick
        ExplicitTop = 291
        ExplicitWidth = 175
      end
    end
  end
end
