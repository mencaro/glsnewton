object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'Boom!'
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
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 857
    Height = 601
    Camera = GLCamera1
    Buffer.BackgroundColor = clBackground
    FieldOfView = 161.106277465820300000
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Button1: TButton
    Left = 96
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Boom'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 184
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Rebuild Bench'
    TabOrder = 2
    OnClick = Button2Click
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 8
  end
end
