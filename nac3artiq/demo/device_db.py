# python demo.py
# artiq_run module.elf

device_db = {
    "core": {
        "type": "local",
        "module": "artiq.coredevice.core",
        "class": "Core",
        "arguments": {
            "host": "kc705",
            "ref_period": 1e-9,
            "ref_multiplier": 8,
            "target": "riscv"
        }
    },
}
