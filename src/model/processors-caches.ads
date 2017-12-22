with Boards;

private package Processors.Caches is
    -- Returns true if the address is in the cache.
    function Check_Cache (
        Which : in Cache_Contents;
        Address : in Address_Type) return Boolean;

    -- Adds the Address:Value pair to the cache, possibly flushing it
    procedure Add_To_Cache (
        Kind : in Boards.Upgrade_Level;
        Which : in out Cache_Contents;
        Address : in Address_Type;
        Memory : in Memory_Array);
end Processors.Caches;
