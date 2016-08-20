{$IF FPC_FULLVERSION < 30000}
{$ERROR Freepascal 3.0.0 + required}
{$ENDIF}

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}
{$SCOPEDENUMS ON}
{$INLINE ON}

{$ifdef ENDIAN_BIG}
  {$warning big-endian is untested}
{$endif}
