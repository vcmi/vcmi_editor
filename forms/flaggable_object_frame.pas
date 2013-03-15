unit flaggable_object_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, base_object_options_frame;

type

  { TFlaggableFrame }

  TFlaggableFrame = class(TBaseObjectOptionsFrame)
    lbOwner: TLabel;
    edOwnerRG: TRadioGroup;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

