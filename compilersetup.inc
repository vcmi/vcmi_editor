{$IF FPC_FULLVERSION < 20604}
{$ERROR Freepascal 2.6.4 + required}
{$ENDIF}

{$mode objfpc}{$H+}
{.$modeswitch advancedrecords}
{$SCOPEDENUMS ON}
{$INLINE ON}

{$ifdef ENDIAN_BIG}
  {$warning big-endian is untested}
{$endif}
