program MaterialEditor;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FRMaterialPreview in 'FRMaterialPreview.pas' {RMaterialPreview: TFrame},
  FRColorEditor in 'FRColorEditor.pas' {RColorEditor: TFrame},
  FRTrackBarEdit in 'FRTrackBarEdit.pas' {RTrackBarEdit: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
