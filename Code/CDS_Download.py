#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 21 18:11:36 2025

@author: connor
"""
import cdsapi
import os
import xarray as xr
from concurrent.futures import ThreadPoolExecutor, as_completed


client = cdsapi.Client()
output_dir = "/Users/connor/Library/CloudStorage/Dropbox/AIMS/DORegimes/Data/MW/ERA-5"
merged_path = os.path.join(output_dir, "era5land_merged_2021_2025.nc")

years = range(2021, 2025)
months = range(1, 13)
tasks = [(year, month) for year in years for month in months]

def download_month(year, month):
    month_str = f"{month:02d}"
    filename = f"era5land_{year}_{month_str}.nc"
    target_path = os.path.join(output_dir, filename)

    if os.path.exists(target_path):
        print(f"✓ Already exists: {filename}")
        return target_path

    print(f"↓ Downloading: {filename}")
    client.retrieve(
        "reanalysis-era5-land",
        {
            "product_type": "reanalysis",
            "format": "netcdf",
            "variable": [
                "2m_temperature",
                "surface_solar_radiation_downwards",
                "surface_pressure",
                "total_precipitation"
            ],
            "year": str(year),
            "month": f"{month:02d}",
            "day": [f"{d:02d}" for d in range(1, 32)],
            "time": [f"{h:02d}:00" for h in range(24)],
            "area": [42.9, -112.6, 42.7, -112.4]  # [North, West, South, East]
        },
        target_path
    )
    print(f"✔ Finished: {filename}")
    return target_path

# Step 1: Parallel download
MAX_WORKERS = 4
downloaded_files = []

with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
    futures = [executor.submit(download_month, y, m) for y, m in tasks]
    for future in as_completed(futures):
        try:
            path = future.result()
            downloaded_files.append(path)
        except Exception as e:
            print(f"✗ Error: {e}")

# Step 2: Merge downloaded files
print("🔄 Merging NetCDF files...")

# Ensure sorted by time
downloaded_files.sort()

# Open and merge using xarray
datasets = [xr.open_dataset(fp) for fp in downloaded_files]
merged = xr.concat(datasets, dim="time")

# Save merged dataset
merged.to_netcdf(merged_path)
print(f"✅ Merged NetCDF saved to: {merged_path}")