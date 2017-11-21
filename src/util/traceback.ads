with Ada.Exceptions;

package Traceback is
    procedure Call_Stack;
    procedure Exception_Stack (E : Ada.Exceptions.Exception_Occurrence);
end Traceback;
