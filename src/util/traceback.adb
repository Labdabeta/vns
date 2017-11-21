with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

package body Traceback is
    procedure Call_Stack is
        Trace  : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
        Length : Natural;
    begin
        GNAT.Traceback.Call_Chain (Trace, Length);
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (
            Trace (1 .. Length)));
    end Call_Stack;

    procedure Exception_Stack (E : Ada.Exceptions.Exception_Occurrence) is
    begin
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
    end Exception_Stack;

end Traceback;
