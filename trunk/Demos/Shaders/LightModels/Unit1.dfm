object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Light Models'
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
    Top = 41
    Width = 862
    Height = 568
    Camera = GLCamera1
    FieldOfView = 160.030075073242200000
    Align = alClient
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    Caption = 'Lighting models'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Default (FFP)'
      'Lambert'
      'Blinn'
      'Phong')
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {000000000000803F0000A0C00000803F}
    end
    object GLSphere1: TGLSphere
      Material.FrontProperties.Ambient.Color = {0000803F0000803FDBDADA3E0000803F}
      Material.FrontProperties.Diffuse.Color = {FCFB7B3FC1C0403F8180003E0000803F}
      Material.FrontProperties.Emission.Color = {F2F1713FD8D7573F000000000000803F}
      Position.Coordinates = {000040C000004040000040C00000803F}
      Radius = 0.200000002980232200
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 56
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Light Models - %FPS'
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
    Top = 8
  end
end
