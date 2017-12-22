with Boards; use Boards;

package body Processors.Branches is
    -- Returns true if the branch should probably be taken, false otherwise
    function Predict_Branch (
        Kind : in Upgrade_Level;
        Which : in Branch_State) return Boolean is
    begin
        case Kind is
            when BT_NONE | BT_PERFECT => return False;
            when BT_ONE_BIT => return Which.Last_Direction = DIR_TAKEN;
            when BT_TWO_BIT => return Which.Prediction.Direction = DIR_TAKEN;
            when BT_TWO_LEVEL_TWO_BIT =>
                return Which.Prediction_History (
                    Which.Even_Earlier_Direction,
                    Which.Last_Direction).Direction = DIR_TAKEN;
        end case;
    end Predict_Branch;

    -- Register a 'TAKEN' with a 2-state predictor
    procedure Advance_Prediction (Which : in out Two_State_Branch) is begin
        if Which.Strength = WEAKLY then
            if Which.Direction = DIR_TAKEN then
                Which.Strength := STRONGLY;
            else
                Which.Direction := DIR_TAKEN;
            end if;
        else
            if Which.Direction /= DIR_TAKEN then
                Which.Strength := WEAKLY;
            end if;
        end if;
    end Advance_Prediction;

    -- Register a 'NOT TAKEN' with a 2-state predictor
    procedure Retreat_Prediction (Which : in out Two_State_Branch) is begin
        if Which.Strength = WEAKLY then
            if Which.Direction = DIR_TAKEN then
                Which.Direction := DIR_NOT_TAKEN;
            else
                Which.Strength := STRONGLY;
            end if;
        else
            if Which.Direction = DIR_TAKEN then
                Which.Strength := WEAKLY;
            end if;
        end if;
    end Retreat_Prediction;

    -- Updates all predictors, even though at any point only one updates
    procedure Update_Prediction (
        Which : in out Branch_State;
        Taken : in Boolean) is
    begin
        if Taken then
            Advance_Prediction (Which.Prediction);
            Advance_Prediction (Which.Prediction_History (
                Which.Even_Earlier_Direction, Which.Last_Direction));
        else
            Retreat_Prediction (Which.Prediction);
            Retreat_Prediction (Which.Prediction_History (
                Which.Even_Earlier_Direction, Which.Last_Direction));
        end if;
        Which.Even_Earlier_Direction := Which.Last_Direction;
        if Taken then
            Which.Last_Direction := DIR_TAKEN;
        else
            Which.Last_Direction := DIR_NOT_TAKEN;
        end if;
    end Update_Prediction;
end Processors.Branches;
