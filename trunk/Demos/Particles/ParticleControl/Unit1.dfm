object Form1: TForm1
  Left = 192
  Top = 107
  Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1089#1074#1086#1081#1089#1090#1074#1072#1084#1080' '#1086#1090#1076#1077#1083#1100#1085#1099#1093' '#1095#1072#1089#1090#1080#1094
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 862
    Height = 609
    Camera = GLCamera1
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 10.000000000000000000
    Buffer.FogEnvironment.FogEnd = 100.000000000000000000
    Buffer.FogEnvironment.FogMode = fmExp2
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clNavy
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roTwoSideLighting]
    Buffer.Lighting = False
    FieldOfView = 143.638885498046900000
    Align = alClient
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 150
    Height = 17
    Caption = 'Particles count=10'
    TabOrder = 1
  end
  object TrackBar2: TTrackBar
    Left = 8
    Top = 31
    Width = 150
    Height = 26
    Max = 100
    Min = 1
    PageSize = 1
    Position = 1
    TabOrder = 2
    ThumbLength = 16
    OnChange = tbPCChange
  end
  object Panel1: TPanel
    Left = 164
    Top = 8
    Width = 150
    Height = 17
    Caption = 'Max particle size=10'
    TabOrder = 3
  end
  object TrackBar1: TTrackBar
    Left = 164
    Top = 31
    Width = 150
    Height = 26
    Max = 255
    Min = 1
    PageSize = 1
    Position = 10
    TabOrder = 4
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Sorting'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 552
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100000.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000000000A0400000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 552
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1089#1074#1086#1081#1089#1090#1074#1072#1084#1080' '#1086#1090#1076#1077#1083#1100#1085#1099#1093' '#1095#1072#1089#1090#1080#1094' - %FPS'
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
    Left = 88
    Top = 552
  end
end
