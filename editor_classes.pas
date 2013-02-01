unit editor_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TNamedCollection
    Stored as object in JSON
    uses DisplayName as a name of field
  }

  TNamedCollection = class(TCollection)

  end;

  { TArrayCollection
    Stored as array in JSON
  }

  TArrayCollection = class(TCollection)

  end;

implementation

end.

