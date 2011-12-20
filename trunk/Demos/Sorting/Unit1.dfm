object Form1: TForm1
  Left = 192
  Top = 107
  ActiveControl = GLSceneViewer1
  Caption = #1042#1083#1080#1103#1085#1080#1077' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1085#1072' '#1087#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100
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
    Top = 33
    Width = 862
    Height = 576
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 148.167816162109400000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 33
    Align = alTop
    Locked = True
    TabOrder = 1
    object RadioButton1: TRadioButton
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = 'FrontToBack'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 128
      Top = 8
      Width = 113
      Height = 17
      Caption = 'BackToFront'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object CheckBox1: TCheckBox
      Left = 248
      Top = 8
      Width = 97
      Height = 17
      TabStop = False
      Caption = 'Transparency'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 360
      Top = 8
      Width = 97
      Height = 17
      Caption = 'DepthTest'
      TabOrder = 3
      OnClick = CheckBox2Click
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 568
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 82.126426696777340000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000000000000000C00000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLPlane1: TGLPlane
      Material.FrontProperties.Diffuse.Color = {8382823E8F8E8E3EFEFD7D3F0000803F}
      Direction.Coordinates = {000000000000803F000040B300000000}
      PitchAngle = 90.000000000000000000
      Position.Coordinates = {00000000000080BF000000000000803F}
      Up.Coordinates = {00000000000040330000803F00000000}
      Height = 20.000000000000000000
      Width = 20.000000000000000000
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
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = #1042#1083#1080#1103#1085#1080#1077' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1085#1072' '#1087#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' - %FPS'
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
    Left = 80
    Top = 568
  end
end
