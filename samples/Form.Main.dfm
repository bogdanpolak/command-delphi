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
      Top = 55
      Width = 175
      Height = 34
      Align = alTop
      Caption = 'Button2'
      TabOrder = 1
    end
    object Edit1: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 95
      Width = 175
      Height = 21
      Align = alTop
      TabOrder = 2
      Text = 'Edit1'
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 5
      Top = 122
      Width = 175
      Height = 34
      Action = actExecuteTwoCommands
      Align = alTop
      TabOrder = 3
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 172
      Width = 175
      Height = 17
      Margins.Top = 13
      Align = alTop
      Caption = 'Enable command Button2'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object Button4: TButton
      AlignWithMargins = True
      Left = 5
      Top = 212
      Width = 175
      Height = 34
      Margins.Top = 20
      Action = actDiceRolls
      Align = alTop
      TabOrder = 5
    end
    object CheckBox2: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 252
      Width = 175
      Height = 17
      Action = actShowProgressBar
      Align = alTop
      TabOrder = 6
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
  object ActionManager1: TActionManager
    Left = 232
    Top = 176
    StyleName = 'Platform Default'
    object actExecuteTwoCommands: TAction
      Caption = 'Execute Both Commands'
      OnExecute = actExecuteTwoCommandsExecute
    end
    object actDiceRolls: TAction
      Caption = 'Dice Rolls Command'
      OnExecute = actDiceRollsExecute
    end
    object actShowProgressBar: TAction
      Caption = 'Show Progress Bar'
      OnExecute = actShowProgressBarExecute
    end
  end
end
