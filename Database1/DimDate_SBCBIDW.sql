/*
   @Author       Min Wu
   @Created      2021-01-06
   @Purpose      This is a date dimension that we use to store 30 years data since 2005-01-01

*/
USE [SBCBIDW]
GO

/****** To create a date dimention table ******/
DROP TABLE IF EXISTS dbo.#dim
GO

DROP TABLE IF EXISTS dbo.DimDate
GO

-- create other useful index(es) here

DECLARE @StartDate DATE = '20050101', @NumberOfYears INT = 30;

-- prevent set or regional settings from interfering with 
-- interpretation of dates / literals

SET DATEFIRST 7;
SET DATEFORMAT mdy;
SET LANGUAGE US_ENGLISH;

DECLARE @CutoffDate DATE = DATEADD(YEAR, @NumberOfYears, @StartDate);

-- this is just a holding table for intermediate calculations:

CREATE TABLE dbo.#dim (
	[date] DATE PRIMARY KEY, 
	[day] AS DATEPART(DAY, [date]),
	[month] AS DATEPART(MONTH, [date]),
	FirstOfMonth AS CONVERT(DATE, DATEADD(MONTH, DATEDIFF(MONTH, 0, [date]), 0)),
	[MonthName] AS DATENAME(MONTH, [date]),
	[week] AS DATEPART(WEEK, [date]),
	[ISOweek] AS DATEPART(ISO_WEEK, [date]),
	[DayOfWeek] AS DATEPART(WEEKDAY, [date]),
	[quarter] AS DATEPART(QUARTER, [date]),
	[year] AS DATEPART(YEAR, [date]),
	FirstOfYear AS CONVERT(DATE, DATEADD(YEAR,  DATEDIFF(YEAR,  0, [date]), 0)),
	Style112 AS CONVERT(CHAR(8), [date], 112),
	Style101 AS CONVERT(CHAR(10), [date], 101)
);



-- use the catalog views to generate as many rows as we need

INSERT dbo.#dim([date]) 
SELECT d
FROM (
	SELECT d = DATEADD(DAY, rn - 1, @StartDate)
	FROM (
		SELECT TOP (DATEDIFF(DAY, @StartDate, @CutoffDate)) rn = ROW_NUMBER() OVER (ORDER BY s1.[object_id])
		FROM sys.all_objects AS s1
		CROSS JOIN sys.all_objects AS s2
		-- on my system this would support > 5 million days
		ORDER BY s1.[object_id]
	) AS x
) AS y;

CREATE TABLE dbo.DimDate
(
	DateKey INT NOT NULL PRIMARY KEY,
	DT DATE NOT NULL,
	[Day] TINYINT NOT NULL,
	DaySuffix CHAR(2) NOT NULL,
	[Weekday] TINYINT NOT NULL,
	WeekDayName VARCHAR(10) NOT NULL,
	IsWeekend BIT NOT NULL,
	IsHoliday BIT NOT NULL,
	HolidayText VARCHAR(64) SPARSE,
	DOWInMonth TINYINT NOT NULL,
	[DayOfYear] SMALLINT NOT NULL,
	WeekOfMonth TINYINT NOT NULL,
	WeekOfYear TINYINT NOT NULL,
	ISOWeekOfYear TINYINT NOT NULL,
	[Month] TINYINT NOT NULL,
	[MonthName] VARCHAR(10) NOT NULL,
	[Quarter] TINYINT NOT NULL,
	QuarterName VARCHAR(6) NOT NULL,
	[Year] INT NOT NULL,
	MMYYYY CHAR(6) NOT NULL,
	MonthYear CHAR(7) NOT NULL,
	FirstDayOfMonth DATE NOT NULL,
	LastDayOfMonth DATE NOT NULL,
	FirstDayOfQuarter DATE NOT NULL,
	LastDayOfQuarter DATE NOT NULL,
	FirstDayOfYear DATE NOT NULL,
	LastDayOfYear DATE NOT NULL,
	FirstDayOfNextMonth DATE NOT NULL,
	FirstDayOfNextYear DATE NOT NULL,  
	FiscalYear INT NOT NULL,
	FiscalMonth TINYINT NOT NULL,
	FiscalQuarter TINYINT NOT NULL
);

GO

INSERT dbo.DimDate WITH (TABLOCKX)
SELECT
	DateKey = CONVERT(INT, Style112),
	DT = [date],
	[Day] = CONVERT(TINYINT, [day]),
	DaySuffix = CONVERT(CHAR(2), CASE WHEN [day] / 10 = 1 THEN 'th' ELSE CASE RIGHT([day], 1) WHEN '1' THEN 'st' WHEN '2' THEN 'nd' WHEN '3' THEN 'rd' ELSE 'th' END END),
	[Weekday] = CONVERT(TINYINT, [DayOfWeek]),
	[WeekDayName] = CONVERT(VARCHAR(10), DATENAME(WEEKDAY, [date])),
	[IsWeekend] = CONVERT(BIT, CASE WHEN [DayOfWeek] IN (1,7) THEN 1 ELSE 0 END),
	[IsHoliday] = CONVERT(BIT, 0),
	HolidayText = CONVERT(VARCHAR(64), NULL),
	[DOWInMonth] = CONVERT(TINYINT, ROW_NUMBER() OVER (PARTITION BY FirstOfMonth, [DayOfWeek] ORDER BY [date])),
	[DayOfYear] = CONVERT(SMALLINT, DATEPART(DAYOFYEAR, [date])),
	WeekOfMonth = CONVERT(TINYINT, DENSE_RANK() OVER (PARTITION BY [year], [month] ORDER BY [week])),
	WeekOfYear = CONVERT(TINYINT, [week]),
	ISOWeekOfYear = CONVERT(TINYINT, ISOWeek),
	[Month] = CONVERT(TINYINT, [month]),
	[MonthName] = CONVERT(VARCHAR(10), [MonthName]),
	[Quarter] = CONVERT(TINYINT, [quarter]),
	QuarterName = CONVERT(VARCHAR(6), CASE [quarter] WHEN 1 THEN 'First' WHEN 2 THEN 'Second' WHEN 3 THEN 'Third' WHEN 4 THEN 'Fourth' END), 
	[Year] = [year],
	MMYYYY = CONVERT(CHAR(6), LEFT(Style101, 2)    + LEFT(Style112, 4)),
	MonthYear = CONVERT(CHAR(7), LEFT([MonthName], 3) + LEFT(Style112, 4)),
	FirstDayOfMonth = FirstOfMonth,
	LastDayOfMonth = MAX([date]) OVER (PARTITION BY [year], [month]),
	FirstDayOfQuarter = MIN([date]) OVER (PARTITION BY [year], [quarter]),
	LastDayOfQuarter = MAX([date]) OVER (PARTITION BY [year], [quarter]),
	FirstDayOfYear = FirstOfYear,
	LastDayOfYear = MAX([date]) OVER (PARTITION BY [year]),
	FirstDayOfNextMonth = DATEADD(MONTH, 1, FirstOfMonth),
	FirstDayOfNextYear = DATEADD(YEAR,  1, FirstOfYear),
	FiscalYear = CONVERT(INT, CASE WHEN [month] > 3 THEN [year]+1 ELSE [year] END), 
	FiscalMonth = CONVERT(TINYINT, CASE WHEN [month] >3 THEN [month]-3 ELSE [month]+9 END), 
	FiscalQuarter = CONVERT(TINYINT, CASE WHEN [quarter] >1 THEN [quarter]-1 ELSE 4 END)
FROM dbo.#dim
OPTION (MAXDOP 1);
GO

--BC holidays
WITH 
	x AS (
		SELECT DateKey, DT, IsHoliday, HolidayText, FirstDayOfYear,
		DOWInMonth, [MonthName], [WeekDayName], [Day], [Year],
		LastDOWInMonth = ROW_NUMBER() OVER (
			PARTITION BY FirstDayOfMonth, [Weekday] 
			ORDER BY DT DESC
		)
		FROM dbo.DimDate
	)
UPDATE x 
SET 
	IsHoliday = 1, 
	HolidayText = CASE
		WHEN (DT = FirstDayOfYear) THEN 'New Year''s Day'
		WHEN ([MonthName] = 'February' AND [WeekDayName] = 'Monday' AND (([DOWInMonth] = 2 AND [Year] < 2019) OR ([DOWInMonth] = 3 AND [Year] >= 2019))) THEN 'BC Family Day'    -- (2nd Monday in February)
		WHEN ([day] > 17 And [day] < 25 AND [MonthName] = 'May' AND [WeekDayName] = 'Monday') THEN 'Victoria Day'    -- (the Monday before May 25)
		WHEN ([MonthName] = 'July' AND [Day] = 1) THEN 'Canada Day'          -- (July 1st)
		WHEN ([DOWInMonth] = 1 AND [MonthName] = 'August' AND [WeekDayName] = 'Monday') THEN 'British Columbia Day'    -- (1st Monday in August)
		WHEN ([DOWInMonth] = 1 AND [MonthName] = 'September' AND [WeekDayName] = 'Monday') THEN 'Labour Day'                -- (first Monday in September)
		WHEN ([DOWInMonth] = 2 AND [MonthName] = 'October' AND [WeekDayName] = 'Monday') THEN 'Thanksgiving'              -- Thanksgiving (second Monday in October)
		WHEN ([MonthName] = 'November' AND [Day] = 11) THEN 'Remembrance Day'            -- Remembrance Day (November 11th)
		WHEN ([MonthName] = 'December' AND [Day] = 25) THEN 'Christmas Day'
		WHEN ([MonthName] = 'December' AND [Day] = 26) THEN 'Boxing Day'
	END
WHERE 
	(DT = FirstDayOfYear)
	OR 
	([MonthName] = 'February' AND [WeekDayName] = 'Monday' AND (([DOWInMonth] = 2 AND [Year] < 2019) OR ([DOWInMonth] = 3 AND [Year] >= 2019)))
	OR 
	([day] > 17 And [day] < 25 AND [MonthName] = 'May' AND [WeekDayName] = 'Monday')
	OR 
	([MonthName] = 'July' AND [Day] = 1)
	OR 
	([DOWInMonth] = 1 AND [MonthName] = 'August' AND [WeekDayName] = 'Monday')
	OR 
	([DOWInMonth] = 1 AND [MonthName] = 'September' AND [WeekDayName] = 'Monday')
	OR 
	([DOWInMonth] = 2 AND [MonthName] = 'October'   AND [WeekDayName] = 'Monday')
	OR 
	([MonthName] = 'November' AND [Day] = 11)
	OR 
	([MonthName] = 'December' AND [Day] = 25)
	OR 
	([MonthName] = 'December' AND [Day] = 26);
GO

UPDATE [dbo].[DimDate] 
SET IsHoliday = 1, HolidayText = 'New Year''s Day(in lieu)'
WHERE 
	([Month] = 12 AND [Day] = 31 AND [Weekday] = 6) -- Friday 12/31 when 1/1 is Saturday
	OR 
	([Month] = 1 AND [Day] = 2 AND [Weekday] = 2);
GO

-- Monday 1/2 when 1/1 is Sunday

UPDATE [dbo].[DimDate] 
SET IsHoliday = 1, HolidayText = 'Canada Day(in lieu)'
WHERE 
	([Month] = 6 AND [Day] = 30 AND [Weekday] = 6) -- Friday 6/30 when 7/1 is Saturday
	OR 
	([Month] = 7 AND [Day] = 2 AND [Weekday] = 2);
GO

-- Monday 7/2 when 7/1 is Sunday

UPDATE [dbo].[DimDate] 
SET IsHoliday = 1, HolidayText = 'Remembrance Day(in lieu)'
WHERE 
	([Month] = 11 AND [Day] = 10 AND [Weekday] = 6) -- Friday 11/10 when 11/11 is Saturday
	OR
	([Month] = 11 AND [Day] = 12 AND [Weekday] = 2);
GO

-- Monday 11/12 when 11/11 is Sunday
   
UPDATE [dbo].[DimDate] 
SET IsHoliday = 1, HolidayText = 'Christmas Day(in lieu)'
WHERE 
	([Month] = 12 AND [Day] = 24 AND [Weekday] = 6) -- Friday 12/24 when 12/25 is Saturday
	OR
	([Month] = 12 AND [Day] = 27 AND [Weekday] = 3);
GO

-- Tuesday 12/27 when 12/25 is Sunday

UPDATE [dbo].[DimDate] 
SET IsHoliday = 1, HolidayText = 'Boxing Day(in lieu)'
WHERE 
	([Month] = 12 AND [Day] = 28 AND [Weekday] = 2) -- Thursday 12/28 when 12/26 is Saturday
	OR
	([Month] = 12 AND [Day] = 27 AND [Weekday] = 2); -- Monday 12/27 when 12/26 is Sunday

-- the function to return the Easter holiday dates for any given year
GO

-- to mark the Easter holidays in the calendar table:
;WITH x AS 
(
	SELECT d.DT, d.IsHoliday, d.HolidayText, h.HolidayName
	FROM dbo.DimDate AS d
	CROSS APPLY dbo.GetEasterHolidays(d.[Year]) AS h
	WHERE d.DT = h.DT
)
UPDATE x SET IsHoliday = 1, HolidayText = HolidayName;
GO

ALTER TABLE [dbo].[DimDate]
ADD
	[SBCQuarter] NVARCHAR(20) NOT NULL DEFAULT '',
	[PSAPayPeriod] NVARCHAR(20) NOT NULL DEFAULT '',
	[WeekName] NVARCHAR(20) NOT NULL DEFAULT '',
	[SDSIWeekType] VARCHAR(255) NULL,
	[LastDayOfPSAPayPeriod] DATE,
	[FirstDayOfPSAPayPeriod] DATE,
	[FirstDayOfWeek] DATE,
	[LastDayOfWeek] DATE;
GO

-- Adds SBC Quarter and the first and last day of the week
UPDATE [dbo].[DimDate] 
SET 
	[SBCQuarter] = 'FY'+ cast(([FiscalYear]-1) as char(4)) +'-'+substring(CAST([FiscalYear] as CHAR(4)), 3, 2)+' QTR'+cast([FiscalQuarter] as char(1)),
	[FirstDayOfWeek] = DATEADD(wk, 0, DATEADD(DAY, 1-DATEPART([WEEKDAY], DT), DATEDIFF(dd, 0, DT))),
	[LastDayOfWeek] = DATEADD(wk, 1, DATEADD(DAY, 0-DATEPART([WEEKDAY], DT), DATEDIFF(dd, 0, DT)));



-- the function to return the first and last day of the PSA pay period
GO

-- sets the first and last day of the PSA pay period
WITH x AS 
(
	SELECT d.DT, d.[LastDayOfPSAPayPeriod], d.[FirstDayOfPSAPayPeriod], p.PSAPeriodStartDate, p.PSAPeriodEndDate
	FROM dbo.DimDate AS d
	CROSS APPLY dbo.GetPSAPayPeriod(d.DT, 28, CAST('2004-12-26' AS DATE)) AS p
	WHERE d.DT = p.[Date]
)
UPDATE x SET LastDayOfPSAPayPeriod = PSAPeriodEndDate, FirstDayOfPSAPayPeriod = PSAPeriodStartDate;
GO

-- sets the PSA pay period field
UPDATE DimDate
SET PSAPayPeriod = 
	CASE -- add leading white space to single digit dates
		WHEN DATEPART(DAY, FirstDayOfPSAPayPeriod) < 10  AND DATEPART(DAY, LastDayOfPSAPayPeriod) < 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfPSAPayPeriod), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, FirstDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfPSAPayPeriod), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, LastDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' ' + -- End Date
				CAST(DATEPART(YEAR, LastDayOfPSAPayPeriod) AS CHAR(4)) -- Year (from end date)
		WHEN DATEPART(DAY, FirstDayOfPSAPayPeriod) >= 10  AND DATEPART(DAY, LastDayOfPSAPayPeriod) < 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfPSAPayPeriod), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, FirstDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfPSAPayPeriod), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, LastDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' ' + -- End Date
				CAST(DATEPART(YEAR, LastDayOfPSAPayPeriod) AS CHAR(4)) -- Year (from end date)
		WHEN DATEPART(DAY, FirstDayOfPSAPayPeriod) < 10  AND DATEPART(DAY, LastDayOfPSAPayPeriod) >= 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfPSAPayPeriod), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, FirstDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfPSAPayPeriod), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, LastDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' ' + -- End Date
				CAST(DATEPART(YEAR, LastDayOfPSAPayPeriod) AS CHAR(4)) -- Year (from end date)
		WHEN DATEPART(DAY, FirstDayOfPSAPayPeriod) >= 10  AND DATEPART(DAY, LastDayOfPSAPayPeriod) >= 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfPSAPayPeriod), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, FirstDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfPSAPayPeriod), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, LastDayOfPSAPayPeriod) AS NVARCHAR(2)) + ' ' + -- End Date
				CAST(DATEPART(YEAR, LastDayOfPSAPayPeriod) AS CHAR(4)) -- Year (from end date)
	END;
GO

-- sets the Weekname
UPDATE DimDate
SET Weekname = 
	CASE -- add leading white space to single digit dates
		WHEN DATEPART(DAY, FirstDayOfWeek) < 10  AND DATEPART(DAY, LastDayOfWeek) < 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfWeek), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, FirstDayOfWeek) AS CHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfWeek), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, LastDayOfWeek) AS CHAR(2)) -- End Date
		WHEN DATEPART(DAY, FirstDayOfWeek) >= 10  AND DATEPART(DAY, LastDayOfWeek) < 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfWeek), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, FirstDayOfWeek) AS CHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfWeek), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, LastDayOfWeek) AS CHAR(2)) -- End Date
		WHEN DATEPART(DAY, FirstDayOfWeek) < 10  AND DATEPART(DAY, LastDayOfWeek) >= 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfWeek), 3) AS CHAR(3)) + '  ' + CAST(DATEPART(DAY, FirstDayOfWeek) AS CHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfWeek), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, LastDayOfWeek) AS CHAR(2)) -- End Date
		WHEN DATEPART(DAY, FirstDayOfWeek) >= 10  AND DATEPART(DAY, LastDayOfWeek) >= 10
			THEN
				CAST(LEFT(DATENAME(MONTH, FirstDayOfWeek), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, FirstDayOfWeek) AS CHAR(2)) + ' - ' + -- Start Date
				CAST(LEFT(DATENAME(MONTH, LastDayOfWeek), 3) AS CHAR(3)) + ' ' + CAST(DATEPART(DAY, LastDayOfWeek) AS CHAR(2)) -- End Date
	END;
GO

CREATE OR ALTER FUNCTION dbo.GetEasterHolidays(@year INT) 
RETURNS TABLE
WITH SCHEMABINDING
AS 
RETURN (
	WITH x AS (
		SELECT DT = CONVERT(DATE, RTRIM(@year) + '0' + RTRIM([Month]) + RIGHT('0' + RTRIM([Day]),2))
		FROM (SELECT [Month], [Day] = DaysToSunday + 28 - (31 * ([Month] / 4))
		FROM (SELECT [Month] = 3 + (DaysToSunday + 40) / 44, DaysToSunday
		FROM (SELECT DaysToSunday = paschal - ((@year + @year / 4 + paschal - 13) % 7)
		FROM (SELECT paschal = epact - (epact / 28)
		FROM (SELECT epact = (24 + 19 * (@year % 19)) % 30) 
			AS epact) AS paschal) AS dts) AS m) AS d
	)
	SELECT DT, HolidayName = 'Easter Sunday' 
	FROM x
	UNION ALL SELECT DATEADD(DAY,-2,DT), 'Good Friday'   FROM x
	UNION ALL SELECT DATEADD(DAY, 1,DT), 'Easter Monday' FROM x
);
GO

CREATE OR ALTER FUNCTION dbo.GetPSAPayPeriod(@d DATE, @pl INT, @psd DATE) 
RETURNS TABLE
WITH SCHEMABINDING
AS 
RETURN (
	WITH 
		DaysFromFirstPeriod AS (
			SELECT DATEDIFF(DAY, @psd, @d) AS DaysFromFirstPeriod
		),
		DaysIntoPeriod AS (
			SELECT DaysFromFirstPeriod % @pl AS DaysIntoPeriod FROM DaysFromFirstPeriod
		)
	SELECT @d AS [Date], DATEADD(DAY, -1*DaysIntoPeriod, @d) AS PSAPeriodStartDate, DATEADD(DAY, @pl-DaysIntoPeriod-1, @d) AS PSAPeriodEndDate 
	FROM DaysIntoPeriod
);

GO
