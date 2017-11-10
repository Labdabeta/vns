with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

package body FS_Utils is
    function Read_Entire_File (Name : String) return String is
        File_Size : Natural := Natural (Ada.Directories.Size (Name));
        subtype File_Content_String is String (1 .. File_Size);
        package File_String_IO is new Ada.Direct_IO (File_Content_String);
        File : File_String_IO.File_Type;
        Contents : File_Content_String;
    begin
        File_String_IO.Open (File, File_String_IO.In_File, Name);
        File_String_IO.Read (File, Contents);
        File_String_IO.Close (File);

        return Contents;
    end Read_Entire_File;

    function Open_File_Stream (Name : String; File : out Ada.Text_IO.File_Type)
        return Ada.Text_IO.Text_Streams.Stream_Access is
    begin
        if Ada.Directories.Exists (Name) then
            Ada.Text_IO.Open (File, Ada.Text_IO.Out_File, Name);
        else
            Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Name);
        end if;

        return Ada.Text_IO.Text_Streams.Stream (File);
    end Open_File_Stream;
end FS_Utils;
