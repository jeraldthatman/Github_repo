PPI_industry_data = {'Total manufacturing industries':'PCUOMFG--OMFG--',
                    'Electric power distribution':'PCU221122221122',
                    'Fruit and vegetable canning':'PCU311421311421',
                    'Petroleum lubricating oil & grease mfg':'PCU324191324191',
                    'Industrial gas manufacturing':'PCU325120325120',
                    'Chemical mfg':'PCU325---325---',
                    'Carbon black':'PCU3251803251806',
                    'All other basic organic chemical mfg, primary products':'PCU325199325199P',
                    'Plastics material and resins manufacturing':'PCU325211325211',
                    'Synthetic rubber mfg, primary products':'PCU325212325212P',
                    'Pharmaceutical preparation mfg':'PCU325412325412',
                    'Paint & coating mfg':'PCU325510325510',
                    'Iron & steel mills':'PCU331110331110',
                    'Aluminum sheet, plate & foil mfg':'PCU331315331315',
                    'Sheet metal work mfg':'PCU332322332322',
                    'Hardware mfg':'PCU332510332510',
                    'Other fabricated wire product mfg':'PCU332618332618',
                    'Industrial valve mfg':'PCU332911332911',
                    'Ball and roller bearing mfg':'PCU332991332991',
                    'Construction machinery mfg':'PCU333120333120',
                    'Oil & gas field machinery & equipment mfg':'PCU333132333132',
                    'Turbine & turbine generator set unit mfg':'PCU333611333611',
                    'Capacitor, resistor, coil, transformer, and other mfg':'PCU33441K33441K',
                    'Electric power and specialty transformer mfg':'PCU335311335311',
                    'Switchgear & switchboard apparatus mfg':'PCU335313335313',
                    'Current-carrying wiring device mfg':'PCU335931335931',
                    'Automobile, light truck and utility vehicle mfg':'PCU336110336110',
                    'Aircraft engine & engine parts mfg':'PCU336412336412',
                    'Other aircraft parts & equipment mfg':'PCU336413336413',
                    'Wholesale distribution of drugs and druggists sundries':'PCU4240004240002',
                    'Wholesale distribution of grocery and related products':'PCU4240004240004',
                    'New car dealers':'PCU441110441110',
                    'Scheduled passenger air transportation':'PCU481111481111',
                    'Line-haul railroads':'PCU482111482111',
                    'Supermarkets and other grocery (exc convenience) stores':'PCU445110445110',
                    'Truck transportation':'PCU484---484---',
                    'General freight trucking, long-distance, truckload':'PCU4841214841212',
                    'Other heavy machinery rental & leasing':'PCU532412532412',
                    'Offices of physicians (exc mental health)':'PCU621111621111',
                    'General medical and surgical hospitals':'PCU622110622110'}

FoodAndGas = {'Bacon':'APU0000704111', 
                'Bananas':'APU0000711211',
                'Bread':'APU0000702111', 
                'Chicken':'APU0000706111',
                'Coffee':'APU0000717311',
                'ChoclateChipCookies':'APU0000702421',
                'Eggs':'APU0000708111',
                'Flour':'APU0000701111',
                'Milk':'APU0000709112',
                'Oranges':'APU0000711311',
                'Rice':'APU0000701312',
                'Tomato':'APU0000712311', 
                'Electricity':'APU000072610', 
                'Fuel':'APU000072511',
                'Gasoline':'APU00007471A',
                'Gasoline_Unleaded':'APU000074714'}

Layoff_events = {'Mass_Layoff_events':'MLUMS00NN0001003'}

CPI = {
                'CPI for All Urban Consumers (CPI-U) 1982-84=100 (Unadjusted)' :'CUUR0000SA0',
                'CPI for All Urban Consumers (CPI-U) 1967=100 (Unadjusted)' :'CUUR0000AA0',
                'CPI for Urban Wage Earners and Clerical Workers (CPI-W) 1982-84=100 (Unadjusted)':'CWUR0000SA0',
                'CPI-U/Less Food and Energy (Unadjusted) ':'CUUR0000SA0L1E',
                'CPI-W/Less Food and Energy (Unadjusted) ':'CWUR0000SA0L1E',
                'Used_Cars_Sales':'CUSR0000SETA02',
                'PPI Final Demand (Seasonally Adjusted) ':'WPSFD4',
                'PPI Final Demand (Unadjusted) ':'WPUFD4',
                'PPI Final Demand less foods and energy (Unadjusted) ':'WPUFD49104',
                'PPI Final Demand less foods, energy, and trade services (Unadjusted) ':'WPUFD49116',
                'PPI Finished Goods 1982=100 (Unadjusted) ':'WPUFD49207',
                'Imports_All Commodities ':'EIUIR',
                'Exports_All Commodities ':'EIUIQ', 
                }


Employment = {
 'Civilian Labor Force (Seasonally Adjusted)': 'LNS11000000',
 'Civilian Employment (Seasonally Adjusted)': 'LNS12000000',
 'Civilian Unemployment (Seasonally Adjusted)': 'LNS13000000',
 'Unemployment Rate (Seasonally Adjusted)': 'LNS14000000',
 'Total Nonfarm Employment: Seasonally Adjusted': 'CES0000000001',
 'Total Private Average Weekly Hours of All Employees: Seasonally Adjusted': 'CES0500000002',
 'Total Private Average Weekly Hours of Prod. and Nonsup. Employees: Seasonally Adjusted': 'CES0500000007',
 'Total Private Average Hourly Earnings of All Employees: Seasonally Adjusted': 'CES0500000003',
 'Total Private Average Hourly Earnings of Prod. and Nonsup. Employees: Seasonally Adjusted': 'CES0500000008'}
Productivity = {
 'Output Per Hour: Non-farm Business Productivity': 'PRS85006092',
 'Nonfarm Business Unit Labor Costs': 'PRS85006112',
 'Nonfarm Business Real Hourly Compensation': 'PRS85006152',
 'Private Nonfarm Business: Multifactor Productivity, annual index': 'MPU4910012'}



from statsmodels.tsa.seasonal import seasonal_decompose
import matplotlib.pyplot as plt

def ts_analysis(df, n_periods = 252, show_plots = True):
    "Return time series decomposition for dataframe. Set Show_plots = False to return data"
    df = df.dropna().sort_index().copy()
    assert n_periods < len(df), "Number of Periods given is greater than the data given "
    # Additive Decomposition
    additive = seasonal_decompose(df, model='additive', extrapolate_trend='freq',period=n_periods)
    # Multiplicative Decomposition 
    multiplicative = seasonal_decompose(df, model='multiplicative', extrapolate_trend='freq', period=n_periods)
    if show_plots != True:
        return additive, multiplicative
    else:
        fig, axes = plt.subplots(4,2, figsize = (15,8))
        axes[0,0].plot(additive.observed, color = 'Black')
        axes[1,0].plot(additive.trend, color = 'orange')
        axes[2,0].plot(additive.seasonal, color = 'green')
        axes[3,0].plot(additive.resid, color = 'red')
        axes[0,1].plot(multiplicative.observed, color = 'Black')
        axes[1,1].plot(multiplicative.trend, color = 'orange')
        axes[2,1].plot(multiplicative.seasonal, color = 'green')
        axes[3,1].plot(multiplicative.resid, color = 'red')

        axes[0,0].set_title("Additive")
        axes[1,0].set_title("Trend")
        axes[2,0].set_title("Seasonality")
        axes[3,0].set_title("Residuals")
        axes[0,1].set_title("Multiplicative")
        axes[1,1].set_title("Trend")
        axes[2,1].set_title("Seasonality")
        axes[3,1].set_title("Residuals")

        fig.suptitle('Time Series Decomposition')
        fig.autofmt_xdate()
        plt.tight_layout()
        plt.show()



National_Accounts = {'PersonalConsumptionExpenditure': ''}