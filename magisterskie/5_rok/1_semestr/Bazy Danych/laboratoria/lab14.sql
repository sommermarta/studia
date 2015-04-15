/* Let us assume we need to frequently analyse the data on the 
volume of ordered products. This has to be made in terms of … products, 
product categories, customers and their location, order dates and ship dates */

/* First let us recreate the table of:
1. products (combined with categories),
2. customers 
3. order_facts (a combination of data from orders and order details) */


CREATE TABLE [dbo].[Customers](
	[CustomerID] [nchar](5) NOT NULL,
	[CompanyName] [nvarchar](40) NOT NULL,
	[ContactName] [nvarchar](30) NULL,
	[ContactTitle] [nvarchar](30) NULL,
	[Address] [nvarchar](60) NULL,
	[City] [nvarchar](15) NULL,
	[Region] [nvarchar](15) NULL,
	[PostalCode] [nvarchar](10) NULL,
	[Country] [nvarchar](15) NULL,
	[Phone] [nvarchar](24) NULL,
	[Fax] [nvarchar](24) NULL,
 CONSTRAINT [PK_Customers] PRIMARY KEY CLUSTERED 
(
	[CustomerID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO


CREATE TABLE [dbo].[Products](
	-- let us remove IDENTITY setting, as the data will be imported with existing PK values
	[ProductID] [int]  NOT NULL,
	[ProductName] [nvarchar](40) NOT NULL,
	-- to simplify the scenario, we will not include Suppliers
	-- in reality the table (and foreign key) would be used
	-- [SupplierID] [int] NULL,
	-- instead of CategoryId, we put CategoryName
	[CategoryName] [nvarchar] (15) NULL,
	[QuantityPerUnit] [nvarchar](20) NULL,
	[UnitPrice] [money] NULL,
	[UnitsInStock] [smallint] NULL,
	[UnitsOnOrder] [smallint] NULL,
	[ReorderLevel] [smallint] NULL,
	[Discontinued] [bit] NOT NULL,
 CONSTRAINT [PK_Products] PRIMARY KEY CLUSTERED 
(
	[ProductID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]



go

CREATE TABLE [dbo].[Order_facts](
	-- let us remove IDENTITY setting, as the data will be imported with existing PK values
	[OrderID] [int] NOT NULL,
	[CustomerID] [nchar](5) NULL,
	-- to simplify the scenario, we will not include EmployeeId
	-- in reality the table (and foreign key) would be used
	-- [EmployeeID] [int] NULL,
	[OrderDate] [datetime] NULL,
	[RequiredDate] [datetime] NULL,
	[ShippedDate] [datetime] NULL,
	-- to simplify the scenario, we will not include EmployeeId
	-- in reality the table (and foreign key) would be used
	-- [ShipVia] [int] NULL,
	[Freight] [money] NULL,
	[ShipName] [nvarchar](40) NULL,
	[ShipAddress] [nvarchar](60) NULL,
	[ShipCity] [nvarchar](15) NULL,
	[ShipRegion] [nvarchar](15) NULL,
	[ShipPostalCode] [nvarchar](10) NULL,
	[ShipCountry] [nvarchar](15) NULL,
	-- and now let us add the columns from Order Details
	[ProductID] [int] NOT NULL,
	[UnitPrice] [money] NOT NULL,
	[Quantity] [smallint] NOT NULL,
	[Discount] [real] NOT NULL,
 CONSTRAINT [PK_Orders] PRIMARY KEY CLUSTERED 
(
	[OrderID],[productid] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [dbo].[Order_facts]  WITH NOCHECK ADD  CONSTRAINT [FK_Orders_Customers] FOREIGN KEY([CustomerID])
REFERENCES [dbo].[Customers] ([CustomerID])
GO

ALTER TABLE [dbo].[Order_facts] CHECK CONSTRAINT [FK_Orders_Customers]
GO




ALTER TABLE [dbo].[Order_facts]  WITH NOCHECK ADD  CONSTRAINT [FK_Order_Details_Products] FOREIGN KEY([ProductID])
REFERENCES [dbo].[Products] ([ProductID])
GO

ALTER TABLE [dbo].[Order_facts] CHECK CONSTRAINT [FK_Order_Details_Products]
GO


insert into customers select * from Northwind.dbo.customers


insert into Products select ProductID,	ProductName,
		CategoryName,
	QuantityPerUnit,
	UnitPrice,
	UnitsInStock,
	UnitsOnOrder,
	ReorderLevel,
	Discontinued from Northwind.dbo.Products p join Northwind.dbo.Categories c
	on p.categoryid=c.categoryid


insert into order_facts select od.OrderID,
	CustomerID,
	OrderDate,
	RequiredDate,
	ShippedDate,
	Freight,
	ShipName,
	ShipAddress,
	ShipCity,
	ShipRegion,
	ShipPostalCode ,
	ShipCountry,
	ProductID,
	UnitPrice,
	Quantity,
	Discount from northwind.dbo.orders o join northwind.dbo.[order details] od
	on od.orderid=o.orderid