program KeyFrameAnimation;

uses
  Forms,
  Unit1 in 'Unit1.pas'{Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'KeyFrame Animation From 3ds';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
