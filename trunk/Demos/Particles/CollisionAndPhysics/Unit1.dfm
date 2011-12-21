object Form1: TForm1
  Left = 192
  Top = 107
  Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1077' '#1084#1077#1085#1077#1076#1078#1077#1088#1072' '#1095#1072#1089#1090#1080#1094
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
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'ScreenQuad'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Particles'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 40
    Width = 97
    Height = 17
    Caption = 'UseTexture'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object Button1: TButton
    Left = 8
    Top = 63
    Width = 97
    Height = 18
    Caption = 'ActivateFX'
    TabOrder = 4
    OnClick = Button1Click
  end
  object TrackBar1: TTrackBar
    Left = 111
    Top = 31
    Width = 150
    Height = 26
    Min = 5
    PageSize = 1
    Position = 5
    TabOrder = 5
    ThumbLength = 16
    OnChange = tbChange
  end
  object Panel1: TPanel
    Left = 111
    Top = 8
    Width = 150
    Height = 17
    Caption = 'TimeSpeed'
    TabOrder = 6
  end
  object Button2: TButton
    Left = 8
    Top = 80
    Width = 97
    Height = 17
    Caption = 'AddForce'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Panel2: TPanel
    Left = 267
    Top = 8
    Width = 150
    Height = 17
    Caption = 'Particles count x1000'
    TabOrder = 8
  end
  object TrackBar2: TTrackBar
    Left = 267
    Top = 31
    Width = 150
    Height = 26
    Min = 1
    PageSize = 1
    Position = 1
    TabOrder = 9
    ThumbLength = 16
    OnChange = tbPCChange
  end
  object Panel3: TPanel
    Left = 423
    Top = 8
    Width = 150
    Height = 17
    Caption = 'UpateTime, 1/x^2'
    TabOrder = 10
  end
  object TrackBar3: TTrackBar
    Left = 423
    Top = 31
    Width = 150
    Height = 26
    Min = 1
    PageSize = 1
    Position = 5
    TabOrder = 11
    ThumbLength = 16
    OnChange = tbSetUTime
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 104
    Width = 97
    Height = 17
    Caption = 'UseRaycast'
    TabOrder = 12
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 120
    Width = 97
    Height = 17
    Caption = 'Transparency'
    TabOrder = 13
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
      Position.Coordinates = {000000000000A0C0040048420000803F}
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
    FormCaption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1077' '#1084#1077#1085#1077#1076#1078#1077#1088#1072' '#1095#1072#1089#1090#1080#1094' - %FPS'
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
