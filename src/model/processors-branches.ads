with Boards;

private package Processors.Branches is
    function Predict_Branch (
        Kind : in Boards.Branch_Type;
        Which : in Branch_State) return Boolean;

    procedure Advance_Prediction (Which : in out Two_State_Branch);

    procedure Retreat_Prediction (Which : in out Two_State_Branch);

    procedure Update_Prediction (
        Which : in out Branch_State;
        Taken : in Boolean);
end Processors.Branches;
