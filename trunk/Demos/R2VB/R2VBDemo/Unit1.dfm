object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Render To Vertex Buffer'
  ClientHeight = 391
  ClientWidth = 429
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
    Width = 429
    Height = 391
    Camera = GLCamera1
    Buffer.BackgroundColor = clActiveCaption
    FieldOfView = 151.307739257812500000
    Align = alClient
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Wireframe/Fill'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object TrackBar1: TTrackBar
    Left = 111
    Top = 8
    Width = 258
    Height = 17
    Max = 16
    Min = 1
    Position = 1
    TabOrder = 2
    ThumbLength = 16
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 136
    Top = 48
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.009999990463257000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
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
    Left = 104
    Top = 48
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Render To Vertex Buffer - %FPS'
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
