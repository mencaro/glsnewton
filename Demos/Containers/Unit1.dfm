object Form1: TForm1
  Left = 192
  Top = 107
  ActiveControl = GLSceneViewer1
  Caption = 'Containers&Collections'
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
    Top = 35
    Width = 862
    Height = 574
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 148.062545776367200000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 35
    Align = alTop
    TabOrder = 1
    ExplicitTop = -6
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 60
      Height = 13
      Caption = 'Texture size:'
    end
    object Label2: TLabel
      Left = 449
      Top = 12
      Width = 18
      Height = 13
      Caption = '256'
    end
    object CheckBox1: TCheckBox
      Left = 80
      Top = 12
      Width = 73
      Height = 17
      Caption = 'As Viewer'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object TrackBar1: TTrackBar
      Left = 152
      Top = 9
      Width = 281
      Height = 20
      Max = 5
      Position = 1
      TabOrder = 1
      ThumbLength = 16
      OnChange = TrackBar1Change
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 568
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 82.126426696777340000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLDummyCube1
      Position.Coordinates = {00000000000000000000A0400000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
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
    FormCaption = 'Containers&Collections - %FPS'
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
