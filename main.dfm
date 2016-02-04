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
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 683
    Height = 555
    ActivePage = CalculationsSheet
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
      ExplicitLeft = 8
      ExplicitTop = 28
      DesignSize = (
        675
        527)
      object lblPastResultsCount: TLabel
        Left = 3
        Top = 3
        Width = 95
        Height = 13
        Caption = 'lblPastResultsCount'
      end
      object PastResultsView: TListView
        Left = 3
        Top = 22
        Width = 669
        Height = 502
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Date'
            Width = 120
          end
          item
            Caption = '#1'
          end
          item
            Caption = '#2'
          end
          item
            Caption = '#3'
          end
          item
            Caption = '#4'
          end
          item
            Caption = '#5'
          end
          item
            Caption = '#6'
          end
          item
            Caption = 'Bonus'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object CalculationsSheet: TTabSheet
      Caption = 'Calculations'
      ImageIndex = 2
    end
  end
end
