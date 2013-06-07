unit clientui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsWndControl, OverbyteIcsWSocket, StdCtrls, ExtCtrls,
  shapeIntf, shapeCodec, shapeTypes, shapeConsts, Grids, MPHexEditor;

type
  TForm2 = class(TForm)
    txtName: TLabeledEdit;
    txtDob: TLabeledEdit;
    txtAge: TLabeledEdit;
    txtHeight: TLabeledEdit;
    btnSend: TButton;
    WSocket1: TWSocket;
    txtPort: TLabeledEdit;
    txtServer: TLabeledEdit;
    hexViewer: TMPHexEditor;
    Label1: TLabel;
    chkCompressed: TCheckBox;
    chkNamedKey: TCheckBox;
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WSocket1DataSent(Sender: TObject; ErrCode: Word);
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

procedure TForm2.btnSendClick(Sender: TObject);
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

    WSocket1.Proto      := 'udp';
    WSocket1.Addr       := txtServer.Text;     { That's a broadcast  ! }
    WSocket1.Port       := txtPort.Text;
//    WSocket.LocalPort  := LocalPortEdit.Text;
    { UDP is connectionless. Connect will just open the socket }
    WSocket1.Connect;
    WSocket1.Send(stream.Memory, stream.Size);
//    MessageEdit.SelectAll;
//    ActiveControl := MessageEdit;

    hexViewer.LoadFromStream(stream);
    Label1.Caption := 'Total Message Size: ' + IntToStr(stream.Size);
  finally
    stream.Free;
  end;
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

procedure TForm2.WSocket1DataSent(Sender: TObject; ErrCode: Word);
begin
  WSocket1.Close;
end;

end.
