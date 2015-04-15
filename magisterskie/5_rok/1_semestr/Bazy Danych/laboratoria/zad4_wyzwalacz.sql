-- zad4 (w nawi�zaniu do zadania 3)
-- a) Dodaj kolumn� Bonus typu money w tab. Employees.
-- b) Napisz wyzwalacz, kt�ry po dodaniu nowego zam�wienia zaktualizuje wysoko�� premii, 
--    przy czym warto�� premii powinna zosta� zmieniona tylko w przypadku, gdy zachodzi 
--    taka konieczno��
--

CREATE TRIGGER updBonus ON Orders AFTER INSERT
AS
BEGIN
	DECLARE @empl int
	DECLARE @lzam int
	DECLARE k1 CURSOR LOCAL FOR SELECT employeeID FROM inserted
	OPEN k1
	FETCH NEXT FROM k1 INTO @empl

	WHILE @@FETCH_STATUS=0
	BEGIN
		SET @lzam=(SELECT COUNT(*) FROM Employees e 
				   JOIN Orders o ON o.EmployeeID=e.EmployeeID
				   WHERE e.EmployeeID=@empl)

		IF @lzam=1 -- pamietac o pracowniku, ktory nie mial zamowien !!!
		BEGIN
			UPDATE Employees SET Bonus=100
			WHERE EmployeeID=@empl
		END
		ELSE IF @lzam=51
		BEGIN
			UPDATE Employees SET Bonus=150
			WHERE EmployeeID=@empl
		END
		ELSE IF @lzam=101
		BEGIN
			UPDATE Employees SET Bonus=250
			WHERE EmployeeID=@empl
		END

		FETCH NEXT FROM k1 INTO @empl
	END

	CLOSE k1
	DEALLOCATE k1

END

