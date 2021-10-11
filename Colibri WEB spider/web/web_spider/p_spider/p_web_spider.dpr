program p_web_spider;

uses
  Forms,
  u_web_spider in 'u_web_spider.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
