unit Animator;
{-------------------------------------------------------------------}
{                    Unit:    Animator.pas                          }
{                    Project: EPA SWMM                              }
{                    Version: 5.2                                   }
{                    Date:    11/01/21   (5.2.0)                    }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Frame with controls that animate the Study Area Map and         }
{   all Profile plots.                                              }
{-------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.ExtCtrls, Uglobals;

type
  TAnimatorFrame = class(TFrame)
    AnimatorBox: TGroupBox;
    SpeedBar: TTrackBar;
    AnimatorToolBar: TToolBar;
    RewindBtn: TToolButton;
    BackBtn: TToolButton;
    PauseBtn: TToolButton;
    FwdBtn: TToolButton;
    Timer: TTimer;
    procedure BackBtnClick(Sender: TObject);
    procedure RewindBtnClick(Sender: TObject);
    procedure PauseBtnClick(Sender: TObject);
    procedure FwdBtnClick(Sender: TObject);
    procedure SpeedBarChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStatus;
  end;

implementation

{$R *.dfm}

uses Fmain, Ubrowser;

procedure TAnimatorFrame.UpdateStatus;
//-----------------------------------------------------------------------------
// Updates status of the animation controls after a new analysis has been
// made or a new variable was chosen for viewing on the map.
//-----------------------------------------------------------------------------
var
  IsEnabled: Boolean;
begin
  // Controls enabled only if output results exist
  // and there is more than 1 time period.
  IsEnabled := True;
  if not RunFlag then IsEnabled := False
  else if Nperiods = 1 then IsEnabled := False;
  if not IsEnabled then PauseBtnClick(Self);
  RewindBtn.Enabled := IsEnabled;
  BackBtn.Enabled := IsEnabled;
  PauseBtn.Enabled := IsEnabled;
  FwdBtn.Enabled := IsEnabled;
  SpeedBar.Enabled := IsEnabled;
end;


procedure TAnimatorFrame.BackBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the BackBtn. Restarts the animation.
//-----------------------------------------------------------------------------
begin
  Timer.Enabled := True;
end;

procedure TAnimatorFrame.FwdBtnClick(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TAnimatorFrame.PauseBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the PauseBtn. Pauses the animation.
//-----------------------------------------------------------------------------
begin
  Timer.Enabled := False;
  BackBtn.Down := False;
  FwdBtn.Down := False;
end;

procedure TAnimatorFrame.RewindBtnClick(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnClick handler for the RewindBtn. Pauses the animation and resets
//  the current display date to the start of the simulation.
//-----------------------------------------------------------------------------
begin
  PauseBtnClick(Sender);
  Ubrowser.CurrentDateIndex := 0;
  MainForm.DateScrollBar.Position := 0;
  Ubrowser.RefreshTimeListBox(False);
end;

procedure TAnimatorFrame.SpeedBarChange(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnChange handler for the SpeedBar trackbar. Changes the speed of the
//  animation.
//-----------------------------------------------------------------------------
begin
  with SpeedBar do
    Timer.Interval := 100*(Max+1-Position);
end;

procedure TAnimatorFrame.TimerTimer(Sender: TObject);
//-----------------------------------------------------------------------------
//  OnTimer handler for the Timer control. Moves to the next (or previous)
//  simulation time period if the FwdBtn (or BackBtn) is depressed.
//-----------------------------------------------------------------------------
begin
  if FwdBtn.Down then
  begin
    if CurrentPeriod = Nperiods-1 then PauseBtnClick(Sender)
    else Ubrowser.IncreaseElapsedTime;
  end
  else if BackBtn.Down then
  begin
    if CurrentPeriod <= 0 then PauseBtnClick(Sender)
    else Ubrowser.DecreaseElapsedTime;
  end
  else Exit;
end;

end.
