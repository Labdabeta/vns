with Ada.Text_IO.Text_Streams;

package FS_Utils is
    function Read_Entire_File (Name : String) return String;
    function Open_File_Stream (Name : String; File : out Ada.Text_IO.File_Type)
        return Ada.Text_IO.Text_Streams.Stream_Access;
end FS_Utils;
