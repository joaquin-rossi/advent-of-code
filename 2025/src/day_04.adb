with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_04 is
    package Cell_Vec is new Vectors
        (Index_Type => Positive,
         Element_Type => Boolean);
    use Cell_Vec;

    package Grid_Vec is new Vectors
        (Index_Type => Positive,
         Element_Type => Cell_Vec.Vector);
    use Cell_Vec;

    Width  : Integer := 0;
    Height : Integer := 0;
    Grid : Grid_Vec.Vector := Grid_Vec.Empty_Vector;

    function Get(X : Integer; Y : Integer) return Boolean is
    begin
        if not (X in 1 .. Width and Y in 1 .. Height) then
            return False;
        end if;

        return Grid(Y)(X);
    end;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;

    procedure Silver_Count is
        X : Integer;
        Y : Integer;
        Xp : Integer;
        Yp : Integer;
        Neigh : Integer;
    begin
        for Y in 1 .. Height loop
            for X in 1 .. Width loop
                if Get(X, Y) then
                    Neigh := 0;

                    for Yp in -1 .. 1 loop
                        for Xp in -1 .. 1 loop
                            if not (Xp = 0 and Yp = 0) and
                                Get(X + Xp, Y + Yp)
                            then
                                Neigh := Neigh + 1;
                            end if;
                        end loop;
                    end loop;

                    if Neigh < 4 then
                        Silver := Silver + 1;
                    end if;
                end if;
            end loop;
        end loop;
    end;

    procedure Gold_Count is
        X : Integer;
        Y : Integer;
        Xp : Integer;
        Yp : Integer;
        Neigh : Integer;

        Grid_New : Grid_Vec.Vector;
        Grid_Changed : Boolean := True;
    begin
        while Grid_Changed loop
            Grid_New := Grid;
            Grid_Changed := False;

            for Y in 1 .. Height loop
                for X in 1 .. Width loop
                    if Get(X, Y) then
                        Neigh := 0;

                        for Yp in -1 .. 1 loop
                            for Xp in -1 .. 1 loop
                                if not (Xp = 0 and Yp = 0) and
                                    Get(X + Xp, Y + Yp)
                                then
                                    Neigh := Neigh + 1;
                                end if;
                            end loop;
                        end loop;

                        if Neigh < 4 then
                            Gold := Gold + 1;
                            Grid_Changed := True;
                            Grid_New(Y)(X) := False;
                        end if;
                    end if;
                end loop;
            end loop;

            Grid := Grid_New;
        end loop;
    end;

    Line   : Unbounded_String;
begin
    while not End_Of_File loop
        Line := To_Unbounded_String (Get_Line);
        if Width = 0 then
            Width := Length(Line);
        end if;
        Height := Height + 1;

        declare
            I   : Integer;
            Row : Cell_Vec.Vector := Cell_Vec.Empty_Vector;
        begin
            for I in 1 .. Width loop
                Cell_Vec.Append(
                    Row,
                    (case Element(Line, I) is
                        when '@' => True,
                        when '.' => False,
                        when others => raise Constraint_Error)
                );
            end loop;

            Grid_Vec.Append(Grid, Row);
        end;
    end loop;

    Silver_Count;
    Gold_Count;

    Put_Line ("Silver: " & Silver'Image);
    Put_Line ("Gold: " & Gold'Image);
end;
