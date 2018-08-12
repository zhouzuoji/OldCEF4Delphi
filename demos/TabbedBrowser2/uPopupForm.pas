unit uPopupForm;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, Buttons, ExtCtrls, StdCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFView;

type
  TFrmPopupForm = class(TCefFormBase)
    lbl1: TLabel;
    CEFView1: TCEFView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FUrl: string;
    procedure CreateBrowser;
    procedure Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
  protected
    procedure OnCefLibInitialized; override;
  public
    { Public declarations }
  end;

procedure Popup(const url: string);

implementation

{$R *.dfm}

procedure Popup(const url: string);
var
  frm: TFrmPopupForm;
begin
  frm := TFrmPopupForm.Create(nil);
  frm.FUrl := url;
  frm.Show;
end;

{ TFrmPopupForm }

procedure TFrmPopupForm.Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  lbl1.Caption := url;
end;

procedure TFrmPopupForm.Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
begin
  Self.Caption := title;
end;

procedure TFrmPopupForm.CreateBrowser;
begin
  CEFView1.Chromium.OnAddressChange := Chromium_OnAddressChange;
  CEFView1.Chromium.OnTitleChange   := Chromium_OnTitleChange;
  CEFView1.CreateBrowser(FUrl);
end;

procedure TFrmPopupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmPopupForm.OnCefLibInitialized;
begin
  inherited;
  CreateBrowser;
end;

end.
