program GUIRender;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'OpenGL GUI';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
