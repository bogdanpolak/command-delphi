object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 434
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnProgressBar: TPanel
    Left = 0
    Top = 393
    Width = 629
    Height = 41
    Align = alBottom
    Caption = 'pnProgressBar'
    TabOrder = 0
    ExplicitLeft = 192
    ExplicitTop = 232
    ExplicitWidth = 185
    object ProgressBar1: TProgressBar
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 611
      Height = 23
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 12
      ExplicitWidth = 546
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 191
    Top = 3
    Width = 435
    Height = 387
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
    ExplicitLeft = 192
    ExplicitTop = 8
    ExplicitWidth = 337
    ExplicitHeight = 377
  end
  object GroupBoxDiceRolls: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 182
    Height = 387
    Align = alLeft
    Caption = 'Dice Rolls'
    TabOrder = 2
    object btnAsycDiceRollCmd: TButton
      AlignWithMargins = True
      Left = 5
      Top = 58
      Width = 172
      Height = 34
      Align = alTop
      Caption = 'Asynchrous Dice Roll'
      TabOrder = 0
      OnClick = btnAsycDiceRollCmdClick
      ExplicitWidth = 99
    end
    object btnDiceRollCommand: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 172
      Height = 34
      Align = alTop
      Caption = 'Standard Dice Roll'
      TabOrder = 1
      OnClick = btnDiceRollCommandClick
      ExplicitLeft = 7
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 64
    Top = 152
  end
end
