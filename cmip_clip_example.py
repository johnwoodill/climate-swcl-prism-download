import xarray as xr
import rioxarray as rxr
import geopandas as gpd
import numpy as np


def get_cmip(file_):
    # Load cmip data
    print("Loading CMIP data")
    ds = xr.open_dataset(file_)

    # Convert from 0-360 centered 
    ds['lon'] = np.where(ds['lon'] > 180, ds['lon'] - 360, ds['lon'])

    # Filter Oregon Boundary
    ds = ds.where((ds['lon'] >= -125.83811358102986) & (ds['lon'] <= -117.0477057972553), drop=True)
    ds = ds.where((ds['lat'] >= 42.00322135307699) & (ds['lat'] <= 46.24472592595039), drop=True)

    # to_dataframe()
    print("Flatten to dataframe")
    indat = ds.to_dataframe().reset_index()
    
    # Remove bnd 2
    indat = indat[indat['bnds'] == 1]

    # Convert cftime to pandas time
    print("Converting cftime to datetime")
    retdat = indat.assign(time = pd.to_datetime(indat['time'].astype(str)))

    return retdat


# Location of file to process
file_ = 'data/cmip/tas_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc'

# Proc file
outdat = get_cmip(file_)

# Gen filename to save
filename_ = file_.split("/")[-1].split(".")[0]

# Save without index
outdat.to_csv(f"data/{filename_}.csv", index=False)

