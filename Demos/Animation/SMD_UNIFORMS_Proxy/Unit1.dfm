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
    Top = 57
    Width = 862
    Height = 552
    Camera = GLCamera1
    Buffer.BackgroundColor = clNavy
    FieldOfView = 159.463394165039100000
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 57
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 232
      Top = 9
      Width = 6
      Height = 13
      Caption = '1'
    end
    object Label2: TLabel
      Left = 16
      Top = 8
      Width = 208
      Height = 13
      Caption = #1063#1072#1089#1090#1086#1090#1072' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' 1/'#1050#1072#1076#1088#1086#1074' '#1074' '#1089#1077#1082#1091#1085#1076#1091
    end
    object Label3: TLabel
      Left = 272
      Top = 8
      Width = 124
      Height = 13
      Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1089#1084#1077#1085#1099' '#1082#1072#1076#1088#1086#1074
    end
    object Label4: TLabel
      Left = 408
      Top = 8
      Width = 6
      Height = 13
      Caption = '1'
    end
    object Label5: TLabel
      Left = 480
      Top = 8
      Width = 154
      Height = 13
      Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1082#1072#1076#1088#1086#1074' '#1072#1085#1080#1084#1072#1094#1080#1080':'
    end
    object Label6: TLabel
      Left = 656
      Top = 8
      Width = 12
      Height = 13
      Caption = '37'
    end
    object Label7: TLabel
      Left = 480
      Top = 24
      Width = 157
      Height = 13
      Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1084#1086#1076#1077#1083#1077#1081' '#1085#1072' '#1089#1094#1077#1085#1077':'
    end
    object Label8: TLabel
      Left = 656
      Top = 24
      Width = 18
      Height = 13
      Caption = '100'
    end
    object Label9: TLabel
      Left = 480
      Top = 40
      Width = 166
      Height = 13
      Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1087#1086#1083#1080#1075#1086#1085#1086#1074' '#1085#1072' '#1089#1094#1077#1085#1077':'
    end
    object Label10: TLabel
      Left = 656
      Top = 40
      Width = 6
      Height = 13
      Caption = '0'
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 21
      Width = 257
      Height = 25
      Max = 52
      Min = 1
      Position = 25
      TabOrder = 0
      ThumbLength = 16
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 264
      Top = 21
      Width = 201
      Height = 25
      Max = 15
      Position = 5
      TabOrder = 1
      ThumbLength = 16
      OnChange = TrackBar2Change
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
