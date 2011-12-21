object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Voxels'
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
    Top = 41
    Width = 862
    Height = 568
    Camera = GLCamera1
    FieldOfView = 160.030075073242200000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 67
      Height = 13
      Caption = 'Voxels count: '
    end
    object Label2: TLabel
      Left = 83
      Top = 14
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 202
      Top = 14
      Width = 56
      Height = 13
      Caption = 'PolyCounts:'
    end
    object Label4: TLabel
      Left = 264
      Top = 14
      Width = 6
      Height = 13
      Caption = '0'
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 32
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000048430000803F0000803F}
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
    Top = 32
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Voxels - %FPS'
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
    Top = 32
  end
end
