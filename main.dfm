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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 683
    Height = 555
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Combos'
      ExplicitWidth = 281
      ExplicitHeight = 165
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
      object ComboResultBox: TListBox
        Left = 3
        Top = 32
        Width = 669
        Height = 492
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
    end
  end
end
