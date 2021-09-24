# python demo.py
# artiq_run module.elf

core_addr = "192.168.1.50"

device_db = {
    "core": {
        "type": "local",
        "module": "artiq.coredevice.core",
        "class": "Core",
        "arguments": {"host": core_addr, "ref_period": 1e-9}
    }
}
