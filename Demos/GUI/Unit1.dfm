object Form1: TForm1
  Left = 192
  Top = 107
  ActiveControl = GLSceneViewer1
  Caption = 'OpenGL GUI'
  ClientHeight = 620
  ClientWidth = 871
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 871
    Height = 620
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    FieldOfView = 150.323684692382800000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 568
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 82.126426696777340000
      NearPlaneBias = 0.100000001490116100
      Position.Coordinates = {00000000000000000000A0400000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 48
    Top = 568
  end
end
