unit Editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Grids, MPHexEditor, shapeIntf,
  shapeCodec, shapeTypes, shapeConsts;

type
  TForm2 = class(TForm)
    btnEncode: TButton;
    txtName: TLabeledEdit;
    txtDob: TLabeledEdit;
    txtAge: TLabeledEdit;
    txtHeight: TLabeledEdit;
    hexViewer: TMPHexEditor;
    Label1: TLabel;
    btnLoad: TButton;
    btnDecode: TButton;
    btnSave: TButton;
    chkCompressed: TCheckBox;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    chkNamedKey: TCheckBox;
    procedure btnEncodeClick(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure chkCompressedClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkNamedKeyClick(Sender: TObject);
  private
    { Private declarations }
    shape: IShapeMessage;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
procedure TForm2.btnDecodeClick(Sender: TObject);
var
  stream: TMemoryStream;
begin

  stream := TMemoryStream.Create;
  try
    hexViewer.SaveToStream(stream);
    stream.Seek(0, 0);
    shape.ReadFromStream(stream);

    txtName.Text := shape['Name'];
    txtAge.Text := shape['Age'];
    if (shape.ElementByName['DOB'].IsEmpty) then
      txtDOB.Text := ''
    else
      txtDOB.Text := DateTimeToStr(shape['DOB']);
    txtHeight.Text := shape['Height'];

  finally
    stream.Free;
  end;
end;

procedure TForm2.btnEncodeClick(Sender: TObject);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    shape['Name'] := txtName.Text;
    shape['Age'] := txtAge.Text;
    
    if txtDOB.Text <> '' then
    begin
      try
        shape['DOB'] := VarToDateTime(txtDOB.Text);
      except
        shape['DOB'] := Unassigned;
      end;
    end;
    shape['Height'] := txtHeight.Text;

    shape.WriteToStream(stream);
    stream.seek(0, 0);

    hexViewer.LoadFromStream(stream);
    Label1.Caption := 'Total Message Size: ' + IntToStr(stream.Size);
  finally
    stream.Free;
  end;
end;

procedure TForm2.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    hexViewer.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm2.btnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    hexViewer.SaveToFile(SaveDialog1.FileName);
  end;
  
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  stream: TMemoryStream;
  i, k: Integer;
begin

  stream := TMemoryStream.Create;
  try
    shape.WriteToStream(stream);
    i := GetTickCount;
    for k := 1 to 100000 do
    begin
      stream.Seek(0, 0);
      shape.ReadFromStream(stream);
    end;

    ShowMessage(Format('Total %d MS, Decode Performance: %5.2f ms',
      [GetTickCount-i, (GetTickCount-i)/100000]));

  finally
    stream.Free;
  end;
end;

procedure TForm2.chkCompressedClick(Sender: TObject);
begin
  if chkCompressed.Checked then
    shape.Encoding := shape.Encoding + SHAPE_ENCODING_COMPRESSED
  else
    shape.Encoding := shape.Encoding - SHAPE_ENCODING_COMPRESSED;
end;

procedure TForm2.chkNamedKeyClick(Sender: TObject);
begin
  if chkNamedKey.Checked then
    shape.Encoding := shape.Encoding + SHAPE_ENCODING_NAMEDINDEX
  else
    shape.Encoding := shape.Encoding - SHAPE_ENCODING_NAMEDINDEX;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  shape := TShapeMessage.Create;
  // Prepare elements types
  shape.Add('Name', SHAPE_STRING);
  shape.Add('Age', SHAPE_UINT16);
  shape.Add('DOB', SHAPE_DOUBLE);
  shape.Add('Height', SHAPE_SINGLE);

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
//  shape.Free;
end;

end.
