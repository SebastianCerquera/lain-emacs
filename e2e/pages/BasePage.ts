import { Page, Locator } from '@playwright/test';

export class BasePage {
    readonly page: Page;
    readonly cookieInput: Locator;
    readonly submitButton: Locator;

    constructor(page: Page) {
        this.page = page;
        this.cookieInput = page.locator('input[name="firstname"]');
        this.submitButton = page.locator('input[type="submit"]');
    }

    async goto() {
        await this.page.goto('/base.html', { timeout: 60000 });
    }

    async authenticate(cookieValue: string = 'Mickey') {
        await this.cookieInput.fill(cookieValue);
        await this.submitButton.click();
        await this.page.waitForURL(/\/scrum\//);
    }
}
