object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Lotto Combo'
  ClientHeight = 555
  ClientWidth = 683
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 683
    Height = 555
    ActivePage = ResultsSheet
    Align = alClient
    TabOrder = 0
    object ComboSheet: TTabSheet
      Caption = 'Combos'
      DesignSize = (
        675
        527)
      object Label1: TLabel
        Left = 47
        Top = 8
        Width = 29
        Height = 13
        Caption = 'out of'
      end
      object lblComboCount: TLabel
        Left = 213
        Top = 9
        Width = 114
        Height = 13
        Caption = 'number of combinations'
        Visible = False
      end
      object ComboCalculateBtn: TButton
        Left = 132
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Calculate'
        TabOrder = 0
        OnClick = ComboCalculateBtnClick
      end
      object ComboSizeEdit: TEdit
        Left = 3
        Top = 5
        Width = 38
        Height = 21
        TabOrder = 1
        Text = '3'
      end
      object ComboMaxEdit: TEdit
        Left = 82
        Top = 5
        Width = 44
        Height = 21
        TabOrder = 2
        Text = '45'
      end
      object ComboBox: TListBox
        Left = 3
        Top = 32
        Width = 123
        Height = 492
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 3
        OnClick = ComboBoxClick
        OnKeyPress = ComboBoxKeyPress
      end
      object ComboMemo: TMemo
        Left = 132
        Top = 32
        Width = 540
        Height = 492
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 4
      end
    end
    object ResultsSheet: TTabSheet
      Caption = 'Past results'
      ImageIndex = 1
      object PastResultsMemo: TMemo
        Left = 3
        Top = 3
        Width = 669
        Height = 521
        TabOrder = 0
      end
    end
  end
end
