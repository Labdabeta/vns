with Boards; use Boards;

package body Processors.Caches is
    function Check_Cache (
        Which : in Cache_Contents;
        Address : in Address_Type) return Boolean is
    begin
        if Which.Size = CS_NONE then
            return False;
        end if;

        for Index in Which.Data'Range loop
            if Which.Data (Index).Address = Address and
                Which.Data (Index).Is_Loaded
            then
                return True;
            end if;
        end loop;
        return False;
    end Check_Cache;

    procedure Add_To_Cache (
        Kind : in Upgrade_Level;
        Which : in out Cache_Contents;
        Address : in Address_Type;
        Memory : in Memory_Array) is
        New_Entry : Cache_Entry := (
            Address => Address,
            Value => Memory (Address),
            Is_Loaded => True,
            Age => 0);

        type Address_Options_Array is array (Address_Type range <>) of
            Address_Type;
        procedure Replace_Oldest_Entry (Spots : in Address_Options_Array) is
            Oldest_Entry : Address_Type;
            Age : Natural := Natural'Last; -- -1 is an empty slot
        begin
            for Index in Spots'Range loop
                if not Which.Data (Spots (Index)).Is_Loaded then
                    Which.Data (Spots (Index)) := New_Entry;
                    return;
                end if;

                if Which.Data (Spots (Index)).Age < Age then
                    Oldest_Entry := Spots (Index);
                    Age := Which.Data (Spots (Index)).Age;
                end if;
            end loop;

            Which.Data (Oldest_Entry) := New_Entry;
        end Replace_Oldest_Entry;

        -- Number of spots a given address could occupy
        function Cache_Size_Index return Address_Type is
        begin
            case Kind is
                when CT_NONE => return 1;
                when CT_TWO_WAY => return 2;
                when CT_FOUR_WAY => return 4;
                when CT_EIGHT_WAY => return 8;
                when CT_FULLY => return Which.Length;
            end case;
        end Cache_Size_Index;
    begin
        if Which.Size = CS_NONE then
            return;
        end if;

        for Index in Address_Type range 0 .. Which.Length loop
            if Which.Data (Index).Address = Address then
                Which.Data (Index) := New_Entry;
                return;
            end if;
        end loop;

        for Index in Address_Type range 0 .. Which.Length loop
            if Which.Data (Index).Is_Loaded then
                Which.Data (Index).Age := Which.Data (Index).Age + 1;
            end if;
        end loop;

        declare
            Spots : Address_Options_Array (1 .. Cache_Size_Index);
        begin
            for Index in Spots'Range loop
                Spots (Index) := (Address + Index) mod Which.Length;
            end loop;

            Replace_Oldest_Entry (Spots);
        end;
    end Add_To_Cache;
end Processors.Caches;
