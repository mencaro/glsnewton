program Project1;

uses
  FastMM4, Forms,
  DemoUnit in 'DemoUnit.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
