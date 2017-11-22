with Memory; use Memory;
with Boards; use Boards;
with Processors; use Processors;
with Assembly;
with FS_Utils;

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Interfaces;

procedure Assembler is begin
    if Ada.Command_Line.Argument_Count /= 2 then
        Ada.Text_IO.Put_Line (
            Ada.Command_Line.Command_Name & " [source] [output]");
        Ada.Text_IO.Put_Line (
            ASCII.HT & "[source] - The source file to assemble");
        Ada.Text_IO.Put_Line (
            ASCII.HT & "[output] - The output file");
        return;
    end if;

    declare
        Code : Memory_Access := Assembly.Assemble (
            Ada.Command_Line.Argument (1));
        File : Ada.Text_IO.File_Type;
        Stream : Ada.Text_IO.Text_Streams.Stream_Access :=
            FS_Utils.Open_File_Stream (Ada.Command_Line.Argument (2), File);
    begin
        for Index in Unit_Type'Range loop
            Interfaces.Unsigned_32'Write (Stream,
                Interfaces.Unsigned_32 (Code.Lengths (Index)));
            for A in Address_Type range 0 .. Code.Lengths (Index) loop
                Cell'Write (Stream, Code.Data (Index) (A));
            end loop;
        end loop;
        Ada.Text_IO.Close (File);
        Free_Memory (Code);
    end;
end Assembler;
