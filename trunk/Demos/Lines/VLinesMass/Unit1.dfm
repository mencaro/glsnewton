object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Volumetric Lines'
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
    Buffer.BackgroundColor = clNavy
    FieldOfView = 160.030075073242200000
    Align = alClient
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 136
      Top = 17
      Width = 58
      Height = 13
      Caption = 'BeamCount:'
    end
    object Label2: TLabel
      Left = 208
      Top = 17
      Width = 6
      Height = 13
      Caption = '0'
    end
    object LifeTime: TLabel
      Left = 256
      Top = 1
      Width = 40
      Height = 13
      Caption = 'LifeTime'
    end
    object Label3: TLabel
      Left = 465
      Top = 1
      Width = 58
      Height = 13
      Caption = 'BeamSpeed'
    end
    object Label4: TLabel
      Left = 674
      Top = 1
      Width = 60
      Height = 13
      Caption = 'BeamLength'
    end
    object RadioButton3: TRadioButton
      Left = 16
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Red'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton4: TRadioButton
      Left = 80
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Glow'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
    object TrackBar2: TTrackBar
      Left = 238
      Top = 13
      Width = 203
      Height = 24
      Max = 100
      Min = 1
      Frequency = 2
      Position = 30
      TabOrder = 2
      ThumbLength = 16
    end
    object TrackBar3: TTrackBar
      Left = 447
      Top = 13
      Width = 203
      Height = 24
      Max = 500
      Min = 1
      Frequency = 10
      Position = 80
      TabOrder = 3
      ThumbLength = 16
    end
    object TrackBar1: TTrackBar
      Left = 656
      Top = 13
      Width = 203
      Height = 24
      Max = 500
      Frequency = 10
      Position = 70
      TabOrder = 4
      ThumbLength = 16
    end
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000000000000000400000803F}
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
    Left = 24
    Top = 40
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Volumetric Lines - %FPS'
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
    Left = 24
    Top = 72
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 24
    Top = 128
  end
end
