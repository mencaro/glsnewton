object Form1: TForm1
  Left = 192
  Top = 107
  ActiveControl = GLSceneViewer1
  Caption = #1057#1088#1072#1074#1085#1077#1085#1080#1077' '#1087#1088#1086#1080#1079#1074#1086#1076#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1088#1072#1079#1085#1099#1093' '#1090#1080#1087#1086#1074' '#1087#1088#1086#1082#1089#1080
  ClientHeight = 609
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 82
    Width = 862
    Height = 527
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 145.378067016601600000
    Align = alClient
    TabOrder = 0
  end
  object rgProxyType: TRadioGroup
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    Caption = #1058#1080#1087' '#1087#1088#1086#1082#1089#1080
    Columns = 4
    Items.Strings = (
      #1048#1085#1089#1090#1072#1085#1089#1099' ('#1092#1087#1089'+'#1087#1072#1084#1103#1090#1100')'
      #1055#1088#1086#1082#1089#1080'('#1087#1072#1084#1103#1090#1100'+'#1084#1072#1090#1077#1088#1080#1072#1083#1099')'
      'GLSceneProxy')
    TabOrder = 1
    OnClick = rgProxyTypeClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 862
    Height = 41
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 459
      Top = 0
      Width = 64
      Height = 13
      Caption = #1058#1080#1087' '#1086#1073#1098#1077#1082#1090#1072
    end
    object Label2: TLabel
      Left = 400
      Top = 19
      Width = 30
      Height = 13
      Caption = '10000'
    end
    object Label3: TLabel
      Left = 392
      Top = 0
      Width = 53
      Height = 13
      Caption = #1054#1073#1098#1077#1082#1090#1086#1074':'
    end
    object tbObjectsCount: TTrackBar
      Left = 0
      Top = 0
      Width = 378
      Height = 41
      Max = 200
      Min = 1
      Frequency = 2
      Position = 100
      TabOrder = 0
      OnChange = tbObjectsCountChange
    end
    object cbObjectType: TComboBox
      Left = 451
      Top = 17
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = #1060#1088#1080#1092#1086#1088#1084#1072' (366 '#1087#1086#1083#1080#1075#1086#1085#1086#1074')'
      OnChange = cbObjectTypeChange
      OnCloseUp = cbObjectTypeCloseUp
      Items.Strings = (
        #1060#1088#1080#1092#1086#1088#1084#1072' (366 '#1087#1086#1083#1080#1075#1086#1085#1086#1074')'
        #1050#1091#1073' (12 '#1087#1086#1083#1080#1075#1086#1085#1086#1074')'
        #1057#1092#1077#1088#1072' (270 '#1087#1086#1083#1080#1075#1086#1085#1086#1074')'
        #1057#1092#1077#1088#1072' (1054 '#1087#1086#1083#1080#1075#1086#1085#1072')')
    end
    object Button1: TButton
      Left = 643
      Top = 10
      Width = 75
      Height = 25
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object CheckBox1: TCheckBox
    Left = 536
    Top = 41
    Width = 97
    Height = 17
    Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 16
    Top = 568
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 82.126426696777340000
      TargetObject = GLDummyCube1
      Position.Coordinates = {000000000000C8420000A0400000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 568
  end
  object matlib: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Red'
        Material.FrontProperties.Diffuse.Color = {CDCC4C3F0000000000000000295CCF3E}
        Tag = 0
      end
      item
        Name = 'Green'
        Material.FrontProperties.Diffuse.Color = {00000000CDCC4C3F000000006666A63E}
        Tag = 0
      end
      item
        Name = 'Blue'
        Material.FrontProperties.Diffuse.Color = {0000000000000000CDCC4C3F52B89E3E}
        Tag = 0
      end>
    TexturePaths = 'Media'
    Left = 80
    Top = 568
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = '%FPS - '#1057#1088#1072#1074#1085#1077#1085#1080#1077' '#1087#1088#1086#1080#1079#1074#1086#1076#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1088#1072#1079#1085#1099#1093' '#1090#1080#1087#1086#1074' '#1087#1088#1086#1082#1089#1080
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 112
    Top = 568
  end
end
