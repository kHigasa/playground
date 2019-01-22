public enum HttpStatus {
    OK(200), NOT_FOUND(404), INTERNAL_SERVER_ERROR(500);

    private final int value;

    private HttpStatus(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}
