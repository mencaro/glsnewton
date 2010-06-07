object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Demo Newton'
  ClientHeight = 492
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 672
    Height = 492
    Buffer.BackgroundColor = clDefault
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 8
    object GLEarthSkyDome1: TGLEarthSkyDome
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      PitchAngle = 90.000000000000000000
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Bands = <>
      Stars = <>
      SunElevation = 75.000000000000000000
      Turbidity = 15.000000000000000000
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00007041000070410000A0C00000803F}
      SpotCutOff = 180.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    FixedDeltaTime = 0.010000000000000000
    OnProgress = GLCadencer1Progress
    Left = 104
    Top = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 168
    Top = 8
  end
  object Timer2: TTimer
    Interval = 100
    OnTimer = Timer2Timer
    Left = 200
    Top = 8
  end
  object Timer3: TTimer
    OnTimer = Timer3Timer
    Left = 232
    Top = 8
  end
end
