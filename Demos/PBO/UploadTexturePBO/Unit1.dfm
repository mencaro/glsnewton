object Form1: TForm1
  Left = 192
  Top = 107
  Caption = #1057#1088#1072#1074#1085#1077#1085#1080#1077' '#1087#1088#1086#1080#1079#1074#1086#1076#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1090#1077#1082#1089#1090#1091#1088#1099'  FFP/PBO'
  ClientHeight = 609
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 862
    Height = 609
    Camera = GLCamera1
    Buffer.BackgroundColor = clActiveCaption
    FieldOfView = 161.350082397460900000
    Align = alClient
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'LoadFromFFP'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'LoadFromPBO'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 176
    Top = 8
    Width = 281
    Height = 25
    Caption = #1047#1072#1075#1088#1091#1078#1072#1077#1090#1089#1103' '#1090#1077#1082#1089#1090#1091#1088#1072' '#1096#1091#1084#1072' '#1088#1072#1079#1084#1077#1088#1086#1084' 2048'#1093'2048'
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 48
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.009999990463257000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000000000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 48
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = #1057#1088#1072#1074#1085#1077#1085#1080#1077' '#1087#1088#1086#1080#1079#1074#1086#1076#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1090#1077#1082#1089#1090#1091#1088#1099'  FFP/PBO - %FPS'
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
    Left = 72
    Top = 48
  end
end
