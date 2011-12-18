program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Сравнение производительности загрузки текстуры  FFP/PBO';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
