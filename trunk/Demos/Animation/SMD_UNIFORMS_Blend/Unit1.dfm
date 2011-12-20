object Form1: TForm1
  Left = 192
  Top = 107
  Caption = #1057#1082#1077#1083#1077#1090#1085#1072#1103' '#1072#1085#1080#1084#1072#1094#1080#1103' '#1085#1072' GPU'
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
    Top = 73
    Width = 862
    Height = 536
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    FieldOfView = 158.863967895507800000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 73
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 35
      Height = 13
      Caption = 'Frame1'
    end
    object Label2: TLabel
      Left = 816
      Top = 48
      Width = 35
      Height = 13
      Caption = 'Frame2'
    end
    object Label3: TLabel
      Left = 408
      Top = 3
      Width = 57
      Height = 13
      Caption = 'Blend factor'
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 16
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Walk'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Walk'
        'Run'
        'Jump'
        'Long Jump'
        'Head turn')
    end
    object TrackBar1: TTrackBar
      Left = 168
      Top = 16
      Width = 521
      Height = 21
      Max = 100
      TabOrder = 1
      TickStyle = tsNone
    end
    object ComboBox2: TComboBox
      Left = 704
      Top = 16
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 2
      Text = 'Run'
      OnChange = ComboBox2Change
      Items.Strings = (
        'Walk'
        'Run'
        'Jump'
        'Long Jump'
        'Head turn')
    end
    object TrackBar2: TTrackBar
      Left = 48
      Top = 43
      Width = 369
      Height = 21
      Max = 100
      TabOrder = 3
      TickStyle = tsNone
      OnChange = TrackBar2Change
    end
    object TrackBar3: TTrackBar
      Left = 440
      Top = 43
      Width = 369
      Height = 21
      Max = 100
      TabOrder = 4
      TickStyle = tsNone
      OnChange = TrackBar3Change
    end
  end
  object GLScene1: TGLScene
    Left = 136
    Top = 48
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.009999990463257000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000000000000070410000803F}
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
    Left = 104
    Top = 48
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = #1057#1082#1077#1083#1077#1090#1085#1072#1103' '#1072#1085#1080#1084#1072#1094#1080#1103' '#1085#1072' GPU- %FPS'
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
  object OpenDialog1: TOpenDialog
    DefaultExt = 'SMD'
    Filter = '*.smd|*.smd'
    FilterIndex = 0
    Left = 184
    Top = 48
  end
end
