object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Multiple Shader Objects'
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
    Top = 145
    Width = 862
    Height = 464
    Camera = GLCamera1
    FieldOfView = 155.675598144531300000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 145
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 408
      Top = 24
      Width = 3
      Height = 13
    end
    object Label2: TLabel
      Left = 408
      Top = 8
      Width = 89
      Height = 13
      Caption = 'Uniforms warnings:'
    end
    object Memo1: TMemo
      Left = 3
      Top = 3
      Width = 401
      Height = 139
      TabOrder = 0
    end
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
      Position.Coordinates = {000000000000803F0000A0400000803F}
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
    Top = 8
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Multiple Shader Objects - %FPS'
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
