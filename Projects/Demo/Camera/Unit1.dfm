object Form1: TForm1
  Left = 192
  Top = 107
  Width = 870
  Height = 640
  Caption = 'Form1'
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
    Height = 613
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    FieldOfView = 150
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 82.1264266967773
      NearPlaneBias = 0.100000001490116
      Position.Coordinates = {0000000000000000000000C00000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        LightStyle = lsOmni
        SpotCutOff = 180
      end
    end
    object GLCube1: TGLCube
      Position.Coordinates = {0000000000000000000000400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000000000000C00000803F}
      CubeSize = 1
      VisibleAtRunTime = True
    end
    object GLDummyCube2: TGLDummyCube
      CubeSize = 0.5
      object GLArrowLine1: TGLArrowLine
        Material.FrontProperties.Diffuse.Color = {00000000CDCC4C3F000000000000803F}
        Scale.Coordinates = {0000003F0000003F0000003F00000000}
        BottomRadius = 0.100000001490116
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.100000001490116
        BottomArrowHeadRadius = 0.100000001490116
      end
    end
    object GLPlane1: TGLPlane
      Direction.Coordinates = {000000000000803F000040B300000000}
      PitchAngle = 90
      Position.Coordinates = {00000000000080BF000000000000803F}
      Up.Coordinates = {00000000000040330000803F00000000}
      Height = 10
      Width = 10
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
end
