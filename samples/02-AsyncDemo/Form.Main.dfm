object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 585
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
    Top = 430
    Width = 629
    Height = 155
    Align = alBottom
    Caption = ' '
    TabOrder = 0
    ExplicitTop = 424
    object lblProgress3: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 133
      Width = 621
      Height = 13
      Margins.Top = 0
      Margins.Bottom = 8
      Align = alTop
      Alignment = taCenter
      Caption = 'lblProgress3'
      ExplicitWidth = 58
    end
    object lblProgress1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 31
      Width = 621
      Height = 13
      Margins.Top = 0
      Margins.Bottom = 8
      Align = alTop
      Alignment = taCenter
      Caption = 'lblProgress1'
      ExplicitWidth = 58
    end
    object lblProgress2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 82
      Width = 621
      Height = 13
      Margins.Top = 0
      Margins.Bottom = 8
      Align = alTop
      Alignment = taCenter
      Caption = 'lblProgress2'
      ExplicitWidth = 58
    end
    object ProgressBar1: TProgressBar
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 611
      Height = 22
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
      ExplicitTop = 8
    end
    object ProgressBar2: TProgressBar
      AlignWithMargins = True
      Left = 9
      Top = 56
      Width = 611
      Height = 22
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      TabOrder = 1
      ExplicitTop = 39
    end
    object ProgressBar3: TProgressBar
      AlignWithMargins = True
      Left = 9
      Top = 107
      Width = 611
      Height = 22
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Margins.Bottom = 4
      Align = alTop
      TabOrder = 2
      ExplicitTop = 69
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 191
    Top = 3
    Width = 435
    Height = 424
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
    ExplicitHeight = 374
  end
  object GroupBoxDiceRolls: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 182
    Height = 424
    Align = alLeft
    Caption = 'Dice Rolls'
    TabOrder = 2
    ExplicitHeight = 374
    object btnAsycDiceRollCmd: TButton
      AlignWithMargins = True
      Left = 5
      Top = 58
      Width = 172
      Height = 34
      Align = alTop
      Caption = 'Asynchrous Command'
      TabOrder = 1
      OnClick = btnAsycDiceRollCmdClick
    end
    object btnDiceRollCommand: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 172
      Height = 34
      Align = alTop
      Caption = 'Standard Command'
      TabOrder = 0
      OnClick = btnDiceRollCommandClick
    end
    object btnAsycDiceRollCmdTwo: TButton
      AlignWithMargins = True
      Left = 5
      Top = 98
      Width = 172
      Height = 34
      Align = alTop
      Caption = 'Asynchrous Command Extra'
      TabOrder = 2
      OnClick = btnAsycDiceRollCmdTwoClick
    end
    object chkShowProgressPanel: TCheckBox
      AlignWithMargins = True
      Left = 7
      Top = 395
      Width = 168
      Height = 17
      Margins.Left = 5
      Margins.Right = 5
      Margins.Bottom = 10
      Align = alBottom
      Caption = 'Show ProgressBars panel'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chkShowProgressPanelClick
      ExplicitTop = 345
    end
    object btnTermianteAllBackgroundJobs: TButton
      AlignWithMargins = True
      Left = 5
      Top = 345
      Width = 172
      Height = 37
      Margins.Bottom = 10
      Align = alBottom
      Caption = 'Termiante All background Jobs'
      TabOrder = 4
      OnClick = btnTermianteAllBackgroundJobsClick
      ExplicitTop = 295
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 72
    Top = 192
  end
end
