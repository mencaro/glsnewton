object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'PickObject'
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
    Left = 161
    Top = 0
    Width = 701
    Height = 609
    Camera = GLCamera1
    Buffer.BackgroundColor = clActiveCaption
    FieldOfView = 161.350082397460900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 161
    Height = 609
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
  end
  object RadioButton1: TRadioButton
    Left = 168
    Top = 8
    Width = 73
    Height = 17
    Caption = 'inFrustum'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 240
    Top = 8
    Width = 73
    Height = 17
    Caption = 'inRect'
    TabOrder = 3
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 48
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {00000000000000800000803F00000000}
      CubeSize = 1.009999990463257000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
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
    Left = 8
    Top = 48
  end
  object mtl: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Default'
        Tag = 0
      end
      item
        Name = 'Selected'
        Material.FrontProperties.Diffuse.Color = {D1D0D03DCDCC4C3FCDCC4C3E0000803F}
        Tag = 0
      end
      item
        Name = 'Transp'
        Material.FrontProperties.Diffuse.Color = {DDDC5C3ECDCC4C3FE6E5653F1283203F}
        Material.BlendingMode = bmTransparency
        Tag = 0
      end>
    Left = 80
    Top = 48
  end
end
