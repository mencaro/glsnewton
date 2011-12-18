object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'CameraControl'
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
    Top = 0
    Width = 862
    Height = 609
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    FieldOfView = 149.811920166015600000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 82.126426696777340000
      NearPlaneBias = 0.100000001490116100
      Position.Coordinates = {0000000000000000000000C00000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLCube1: TGLCube
      Position.Coordinates = {0000000000000000000000400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000000000000C00000803F}
      CubeSize = 1.000000000000000000
      VisibleAtRunTime = True
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 0.500000000000000000
      object GLArrowLine1: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {00000000CDCC4C3F000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        BottomRadius = 0.100000001490116100
        Height = 1.000000000000000000
        TopRadius = 0.100000001490116100
        TopArrowHeadHeight = 0.500000000000000000
        TopArrowHeadRadius = 0.200000002980232200
        BottomArrowHeadHeight = 0.100000001490116100
        BottomArrowHeadRadius = 0.100000001490116100
      end
    end
    object GLPlane1: TGLPlane
      Direction.Coordinates = {000000000000803F000040B300000000}
      PitchAngle = 90.000000000000000000
      Position.Coordinates = {00000000000080BF000000000000803F}
      Up.Coordinates = {00000000000040330000803F00000000}
      Height = 10.000000000000000000
      Width = 10.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
end
